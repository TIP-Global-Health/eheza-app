<?php

/**
 * @file
 * Contains \HedleyRestfulBulkPhotos.
 */

/**
 * Class HedleyRestfulBulkPhotos.
 *
 * Bulk photo fetch endpoint. Accepts a JSON list of styled-photo URLs and
 * returns a custom binary container so the client can populate its photo
 * cache in one HTTP request instead of one per photo.
 *
 * Response layout:
 *   [8 bytes LE uint64: manifest JSON length M]
 *   [M bytes: UTF-8 manifest JSON]
 *   [remainder: concatenated photo binaries in manifest order]
 *
 * Manifest items: { url, status, [offset, length, mime] } where status is
 * one of ok | missing | forbidden | error. Only ok items contribute bytes.
 */
class HedleyRestfulBulkPhotos extends \RestfulBase implements \RestfulDataProviderInterface {

  /**
   * Hard cap on URLs accepted per request.
   */
  const MAX_BATCH = 200;

  /**
   * Overrides \RestfulBase::controllersInfo().
   */
  public static function controllersInfo() {
    return [
      '' => [
        \RestfulInterface::POST => 'bulkFetch',
      ],
    ];
  }

  /**
   * Implements \RestfulInterface::publicFieldsInfo().
   */
  public function publicFieldsInfo() {
    return [];
  }

  /**
   * Bulk fetch handler. Validates request, builds container, streams response.
   */
  public function bulkFetch() {
    $request = $this->getRequest();
    if (empty($request['photos']) || !is_array($request['photos'])) {
      throw new \RestfulBadRequestException('Missing or invalid "photos" array.');
    }
    if (count($request['photos']) > self::MAX_BATCH) {
      throw new \RestfulBadRequestException(format_string('Exceeded MAX_BATCH (@n).', ['@n' => self::MAX_BATCH]));
    }

    $body = $this->assembleContainer($request['photos']);

    drupal_add_http_header('Content-Type', 'application/octet-stream');
    drupal_add_http_header('Content-Length', (string) strlen($body));
    drupal_send_headers();
    echo $body;
    drupal_exit();
  }

  /**
   * Build the binary container for the given URLs.
   *
   * Public so simpletests can exercise assembly without HTTP routing or
   * response streaming. At MAX_BATCH (200) × ~50KB/photo = ~10MB worst
   * case, in-memory assembly is comfortable on the server.
   *
   * @param string[] $urls
   *   Styled-photo URLs as the client received them in measurement payloads.
   *
   * @return string
   *   The full container: 8-byte length prefix, manifest JSON, photo bytes.
   */
  public function assembleContainer(array $urls) {
    $items = [];
    $offset = 0;
    foreach ($urls as $url) {
      $resolved = $this->resolveStyledUrlToPath($url);
      if ($resolved['status'] !== 'ok') {
        $items[] = ['url' => $url, 'status' => $resolved['status']];
        continue;
      }
      $size = filesize($resolved['path']);
      if ($size === FALSE) {
        $items[] = ['url' => $url, 'status' => 'error'];
        continue;
      }
      $items[] = [
        'url' => $url,
        'status' => 'ok',
        'offset' => $offset,
        'length' => $size,
        'mime' => $resolved['mime'],
        '_path' => $resolved['path'],
      ];
      $offset += $size;
    }
    return $this->assembleFromItems($items);
  }

  /**
   * Build the binary container from a list of pre-resolved items.
   *
   * Public so simpletests can exercise the container-assembly logic
   * without going through itok validation / image-style derivative
   * generation (those depend on Drupal infrastructure that's awkward to
   * exercise from a fresh test database). Each item has a 'status' and,
   * for ok items, '_path', 'mime', 'offset', 'length'.
   *
   * @param array $items
   *   Items with statuses already determined.
   *
   * @return string
   *   The container: 8-byte length prefix + manifest JSON + binaries.
   */
  public function assembleFromItems(array $items) {
    $manifest_items = [];
    foreach ($items as $item) {
      unset($item['_path']);
      $manifest_items[] = $item;
    }
    $manifest = json_encode(['items' => $manifest_items]);

    $output = pack('P', strlen($manifest)) . $manifest;
    foreach ($items as $item) {
      if (isset($item['status']) && $item['status'] === 'ok' && isset($item['_path'])) {
        $output .= file_get_contents($item['_path']);
      }
    }
    return $output;
  }

  /**
   * Resolve a styled-photo URL to its on-disk path, validating itok.
   *
   * Returns an array with one of three statuses:
   *   - 'ok': ['status' => 'ok', 'path' => string, 'mime' => string]
   *   - 'missing': ['status' => 'missing'] — URL is well-formed and itok is
   *     valid, but the underlying source file no longer exists on disk.
   *   - 'error': ['status' => 'error'] — URL doesn't match the styled-image
   *     pattern, references an unknown style, has a missing/wrong itok, or
   *     derivative generation failed despite the source existing.
   *
   * @param string $url
   *   The styled-photo URL.
   *
   * @return array
   *   ['status' => string, 'path' => ?string, 'mime' => ?string].
   */
  public function resolveStyledUrlToPath($url) {
    $parsed = parse_url($url);
    if (empty($parsed['path']) || !preg_match('#/files/styles/([^/]+)/public/(.+)$#', $parsed['path'], $m)) {
      return ['status' => 'error'];
    }
    $style_name = $m[1];
    $source_relative = $m[2];

    $style = image_style_load($style_name);
    if (!$style) {
      return ['status' => 'error'];
    }

    $query = [];
    if (!empty($parsed['query'])) {
      parse_str($parsed['query'], $query);
    }
    $itok = isset($query['itok']) ? $query['itok'] : '';
    $source_uri = 'public://' . $source_relative;
    $expected = image_style_path_token($style_name, $source_uri);
    if (!$itok || !hash_equals($expected, $itok)) {
      return ['status' => 'error'];
    }

    // URL + itok valid. Source file existence determines missing vs error.
    $source_path = drupal_realpath($source_uri);
    if ($source_path === FALSE || !file_exists($source_path)) {
      return ['status' => 'missing'];
    }

    $derivative_uri = image_style_path($style_name, $source_uri);
    $derivative_path = drupal_realpath($derivative_uri);
    if ($derivative_path === FALSE || !file_exists($derivative_path)) {
      // Derivative not generated yet; trigger generation now (mirrors the
      // behavior of the single-photo fetch path the SW takes today).
      if (!image_style_create_derivative($style, $source_uri, $derivative_uri)) {
        return ['status' => 'error'];
      }
      $derivative_path = drupal_realpath($derivative_uri);
      if ($derivative_path === FALSE) {
        return ['status' => 'error'];
      }
    }

    $mime = file_get_mimetype($derivative_uri);
    return [
      'status' => 'ok',
      'path' => $derivative_path,
      'mime' => $mime ? $mime : 'image/jpeg',
    ];
  }

}
