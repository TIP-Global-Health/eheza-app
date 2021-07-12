The files in this directory are plain downloads from the WHO ... so,
basically:

    curl -O http://www.who.int/childgrowth/standards/lhfa_boys_z_exp.txt
    curl -O http://www.who.int/childgrowth/standards/lhfa_girls_z_exp.txt
    curl -O http://www.who.int/childgrowth/standards/wfa_boys_z_exp.txt
    curl -O http://www.who.int/childgrowth/standards/wfa_girls_z_exp.txt
    curl -O http://www.who.int/childgrowth/standards/wfl_boys_z_exp.txt
    curl -O http://www.who.int/childgrowth/standards/wfl_girls_z_exp.txt

Now, those tables are appropriate for looking up integer z-scores. To
calculate fractional z-scores, we need different tables. What we're
doing is basically a translation of the reference code (in R), located
at:

    http://www.who.int/growthref/tools/en/  (for ages 5 - 19)
    http://www.who.int/childgrowth/software/en/ (for under 5)

Unpacking the reference code there gives us the various files of the
form:

    ...anthro.txt
    ...who2007.txt

Additional source for under 5 - https://github.com/dirkschumacher/anthro

In our gulpfile, we'll transform them into some JSON that we can more
easily handle. We then store the JSON in three places:

- In our Javascript `assets` directory, so the frontend can ask for it.
- In a z-score module for the backend, so the backend can use it.
- As an Elm module for testing, so our Elm unit tests can use it.
