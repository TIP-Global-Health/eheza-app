This module calculates Z-Scores on the backend.

To help ensure that the calculations are consistent as between frontend and
backend, our `gulp` process for the frontend will write the same JSON files
to the json directory here as it uses for the frontend. We'll then read those
files at run-time to get our tables.

We'll check the JSON into version control, since that's the easiest way to
make it available for PHP. (The JSON files that the frontend uses will be
on the backend too, but are in different places depending on whether we're
running locally or on Pantheon, so it's probably easier to just check them
in.)

I suppose one alternative would have been to put the Z-Score tables in the
database, but it's probably fine to keep them in memory.
