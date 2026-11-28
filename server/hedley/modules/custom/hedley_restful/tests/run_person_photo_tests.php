<?php

/**
 * @file
 * Standalone tests of which photo measurement sets the person's photo.
 *
 * Runs without Drupal bootstrap: hedley_restful.module is loaded directly.
 * A photo taken at a group session, Nutrition, Well Child or Family Nutrition
 * visit becomes the person's photo only when no photo of that person was
 * taken on a later date. Among photos of the same date, the one uploaded
 * last (the highest nid) wins. Deleted measurements, and measurements saved
 * without an image, never set the person's photo.
 *
 * Run as: php run_person_photo_tests.php
 */

/**
 * The module sends CORS headers at load time unless it runs from the CLI.
 */
function drupal_is_cli() {
  return TRUE;
}

require_once dirname(__DIR__) . '/hedley_restful.module';
require_once dirname(dirname(__DIR__)) . '/hedley_reports/tests/assertions.inc';

$passed = 0;
$failed = 0;

/**
 * Builds a photo measurement in the shape the seam takes.
 *
 * Example: photo(120, '2026-05-10') is an image taken on 10 May 2026, the
 * 120th node uploaded; options override 'deleted' and 'has_photo'.
 */
function photo($nid, $date, array $options = []) {
  return $options + [
    'nid' => $nid,
    'date' => $date,
    'deleted' => FALSE,
    'has_photo' => TRUE,
  ];
}

/**
 * Tells whether the measurement's photo becomes the person's photo.
 */
function becomes_person_photo(array $measurement, array $others) {
  return hedley_restful_photo_becomes_person_photo($measurement, $others);
}

// Criterion 1: a photo becomes the person's photo when no photo of that
// person was taken on a later date.
assert_same(becomes_person_photo(photo(300, '2026-05-10'), [
  photo(100, '2026-03-10'),
  photo(200, '2026-04-10'),
]), TRUE, 'A photo taken at a visit becomes the person\'s photo when no photo of that person was taken on a later date', $passed, $failed);

// The person's first photo ever: nothing else to compare with.
assert_same(becomes_person_photo(photo(100, '2026-05-10'), []), TRUE, 'the person\'s first visit photo becomes the person\'s photo', $passed, $failed);

// A late upload of the newest photo: other photos were uploaded after it
// (higher nids), but all were taken on earlier dates.
assert_same(becomes_person_photo(photo(100, '2026-05-10'), [
  photo(200, '2026-03-10'),
  photo(300, '2026-04-10'),
]), TRUE, 'a photo uploaded before older-dated photos still becomes the person\'s photo when it is the newest by date', $passed, $failed);

// Dates compared across a year boundary.
assert_same(becomes_person_photo(photo(100, '2026-01-01'), [
  photo(200, '2025-12-31'),
]), TRUE, 'a photo taken on 1 Jan becomes the person\'s photo over one taken on 31 Dec of the year before', $passed, $failed);

// A later-dated photo measurement that was deleted is no longer a photo of
// the person, so it does not hold back an earlier one.
assert_same(becomes_person_photo(photo(100, '2026-04-10'), [
  photo(200, '2026-05-10', ['deleted' => TRUE]),
]), TRUE, 'a deleted photo measurement taken on a later date does not stop a photo becoming the person\'s photo', $passed, $failed);

// A later-dated measurement saved without an image holds no photo, so no
// photo was taken on a later date.
assert_same(becomes_person_photo(photo(100, '2026-04-10'), [
  photo(200, '2026-05-10', ['has_photo' => FALSE]),
]), TRUE, 'a photo measurement without an image taken on a later date does not stop a photo becoming the person\'s photo', $passed, $failed);

// Criterion 2: an earlier-dated photo uploaded late does not replace the
// person's newer photo.
assert_same(becomes_person_photo(photo(200, '2026-04-10'), [
  photo(100, '2026-05-10'),
]), FALSE, 'A photo taken on an earlier date than the person\'s newest visit photo does not replace the person\'s photo, even when it is uploaded later', $passed, $failed);

// The newest photo is one of several others, not the last in the list.
assert_same(becomes_person_photo(photo(400, '2026-04-10'), [
  photo(100, '2026-06-10'),
  photo(200, '2026-03-10'),
  photo(300, '2026-02-10'),
]), FALSE, 'a late-uploaded photo does not replace the person\'s photo when any other photo was taken on a later date', $passed, $failed);

// The later photo is one day later only.
assert_same(becomes_person_photo(photo(200, '2026-05-09'), [
  photo(100, '2026-05-10'),
]), FALSE, 'a late-uploaded photo taken one day before the person\'s newest photo does not replace it', $passed, $failed);

// A deleted newest photo does not count, but a live one dated later still
// does.
assert_same(becomes_person_photo(photo(400, '2026-04-10'), [
  photo(100, '2026-06-10', ['deleted' => TRUE]),
  photo(200, '2026-05-10'),
]), FALSE, 'a late-uploaded photo does not replace the person\'s photo when a live photo was taken on a later date, besides a deleted one', $passed, $failed);

// Criterion 3: editing an older visit's photo measurement. The edited
// measurement keeps its nid, which is lower than the newer photo's.
assert_same(becomes_person_photo(photo(100, '2026-04-10'), [
  photo(200, '2026-05-10'),
]), FALSE, 'Editing an older visit\'s photo measurement does not change the person\'s photo', $passed, $failed);

// Editing the newest photo measurement still sets the person's photo.
assert_same(becomes_person_photo(photo(200, '2026-05-10'), [
  photo(100, '2026-04-10'),
]), TRUE, 'editing the person\'s newest visit photo measurement sets the person\'s photo', $passed, $failed);

// Criterion 4: a deleted measurement never sets the person's photo.
assert_same(becomes_person_photo(photo(100, '2026-04-10', ['deleted' => TRUE]), [
  photo(200, '2026-05-10'),
]), FALSE, 'Deleting a photo measurement never makes its photo the person\'s photo', $passed, $failed);

// Not even when it is the person's newest photo.
assert_same(becomes_person_photo(photo(200, '2026-05-10', ['deleted' => TRUE]), [
  photo(100, '2026-04-10'),
]), FALSE, 'deleting the person\'s newest photo measurement does not make its photo the person\'s photo', $passed, $failed);

// Nor when it is the person's only photo.
assert_same(becomes_person_photo(photo(100, '2026-05-10', ['deleted' => TRUE]), []), FALSE, 'deleting the person\'s only photo measurement does not make its photo the person\'s photo', $passed, $failed);

// Nor when it shares the newest date and was uploaded last.
assert_same(becomes_person_photo(photo(200, '2026-05-10', ['deleted' => TRUE]), [
  photo(100, '2026-05-10'),
]), FALSE, 'deleting a photo measurement uploaded last on the newest date does not make its photo the person\'s photo', $passed, $failed);

// Criterion 5: two photos taken on the same date. The one uploaded last
// (the higher nid) becomes the person's photo.
assert_same(becomes_person_photo(photo(200, '2026-05-10'), [
  photo(100, '2026-05-10'),
]), TRUE, 'When two photos were taken on the same date, the one uploaded last becomes the person\'s photo', $passed, $failed);

// The one uploaded first, saved again (edited), does not.
assert_same(becomes_person_photo(photo(100, '2026-05-10'), [
  photo(200, '2026-05-10'),
]), FALSE, 'when two photos were taken on the same date, editing the one uploaded first does not make it the person\'s photo', $passed, $failed);

// Uploaded last on the newest date, with older-dated photos uploaded after.
assert_same(becomes_person_photo(photo(200, '2026-05-10'), [
  photo(100, '2026-05-10'),
  photo(300, '2026-04-10'),
]), TRUE, 'the photo uploaded last on the newest date becomes the person\'s photo, though older-dated photos were uploaded after it', $passed, $failed);

// A same-date photo uploaded later but deleted does not win.
assert_same(becomes_person_photo(photo(100, '2026-05-10'), [
  photo(200, '2026-05-10', ['deleted' => TRUE]),
]), TRUE, 'when the same-date photo uploaded later was deleted, the remaining one becomes the person\'s photo', $passed, $failed);

// A same-date measurement uploaded later but saved without an image does
// not win either.
assert_same(becomes_person_photo(photo(100, '2026-05-10'), [
  photo(200, '2026-05-10', ['has_photo' => FALSE]),
]), TRUE, 'when the same-date measurement uploaded later has no image, the photo becomes the person\'s photo', $passed, $failed);

// Criterion 6: a measurement saved without an image never sets (or clears)
// the person's photo.
assert_same(becomes_person_photo(photo(200, '2026-05-10', ['has_photo' => FALSE]), [
  photo(100, '2026-04-10'),
]), FALSE, 'A photo measurement saved without an image does not clear or change the person\'s photo, as before', $passed, $failed);

assert_same(becomes_person_photo(photo(100, '2026-05-10', ['has_photo' => FALSE]), []), FALSE, 'the person\'s only photo measurement, saved without an image, does not set the person\'s photo', $passed, $failed);

assert_same(becomes_person_photo(photo(100, '2026-05-10', ['has_photo' => FALSE, 'deleted' => TRUE]), []), FALSE, 'a deleted photo measurement without an image does not set the person\'s photo', $passed, $failed);

report_results('Person photo', $passed, $failed);
