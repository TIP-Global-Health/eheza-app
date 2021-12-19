<?php

/**
 * Creates a markdown document based on the parsed documentation.
 *
 * @package Apidoc
 * @version 1.00 (2014-04-04)
 * @license GNU Lesser Public License
 */
class HedleyAdminTextTable {

  /**
   * The source path.
   *
   * @var int
   */
  public $maxlen = 50;

  /**
   * The source path.
   *
   * @var array
   */
  private $data = [];

  /**
   * The source path.
   *
   * @var array
   */
  private $header = [];

  /**
   * The source path.
   *
   * @var array
   */
  private $len = [];

  /**
   * The source path.
   *
   * @var array
   */
  private $align = [
    'name' => 'L',
    'type' => 'C',
  ];

  /**
   * Constructor.
   *
   * @param array $header
   *   The header array [key => label, ...].
   * @param array $content
   *   Content.
   * @param array $align
   *   Alignment options [key => L|R|C, ...].
   */
  public function __construct(array $header = NULL, array $content = [], array $align = []) {
    if ($header) {
      $this->header = $header;
    }
    elseif ($content) {
      foreach ($content[0] as $key => $value) {
        $this->header[$key] = $key;
      }
    }

    foreach ($this->header as $key => $label) {
      $this->len[$key] = strlen($label);
    }

    if (is_array($align)) {
      $this->setAlgin($align);
    }

    $this->addData($content);
  }

  /**
   * Overwrite the alignment array.
   *
   * @param array $align
   *   Alignment optios [key => L|R|C, ...].
   */
  public function setAlgin(array $align) {
    $this->align = $align;
  }

  /**
   * Add data to the table.
   *
   * @param array $content
   *   Content.
   */
  public function addData(array $content) {
    foreach ($content as &$row) {
      foreach ($this->header as $key => $value) {
        if (!isset($row[$key])) {
          $row[$key] = '-';
        }
        elseif (strlen($row[$key]) > $this->maxlen) {
          $this->len[$key] = $this->maxlen;
          $row[$key] = substr($row[$key], 0, $this->maxlen - 3) . '...';
        }
        elseif (strlen($row[$key]) > $this->len[$key]) {
          $this->len[$key] = strlen($row[$key]);
        }
      }
    }

    $this->data = $this->data + $content;
    return $this;
  }

  /**
   * Render the table.
   *
   * @param array $content
   *   Additional table content.
   *
   * @return string
   *   Markdown table.
   */
  public function render(array $content = []): string {
    $this->addData($content);

    $res = $this->renderRow($this->header)
      . $this->renderDelimiter();
    foreach ($this->data as $row) {
      $res .= $this->renderRow($row);
    }

    return $res;
  }

  /**
   * Render a single row.
   *
   * @param array $row
   *   Data.
   *
   * @return string
   *   Markdown table row.
   */
  private function renderRow(array $row): string {
    $res = '|';
    foreach ($this->len as $key => $l) {
      $res .= ' ' . $row[$key] . ($l > strlen($row[$key]) ? str_repeat(' ', $l - strlen($row[$key])) : '') . ' |';
    }

    return $res . "\r\n";
  }

  /**
   * Add a delimiter.
   *
   * @return string
   *   Delimiter.
   */
  private function renderDelimiter(): string {
    $res = '|';
    foreach ($this->len as $key => $l) {
      $res .= (isset($this->align[$key]) && ($this->align[$key] == 'C' || $this->align[$key] == 'L') ? ':' : ' ')
        . str_repeat('-', $l)
        . (isset($this->align[$key]) && ($this->align[$key] == 'C' || $this->align[$key] == 'R') ? ':' : ' ')
        . '|';
    }
    return $res . "\r\n";
  }

}
