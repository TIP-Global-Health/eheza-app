The files in this directory are plain downloads from the WHO ... so,
basically:

    curl -O http://www.who.int/childgrowth/standards/lhfa_boys_z_exp.txt
    curl -O http://www.who.int/childgrowth/standards/lhfa_girls_z_exp.txt
    curl -O http://www.who.int/childgrowth/standards/wfa_boys_z_exp.txt
    curl -O http://www.who.int/childgrowth/standards/wfa_girls_z_exp.txt
    curl -O http://www.who.int/childgrowth/standards/wfl_boys_z_exp.txt
    curl -O http://www.who.int/childgrowth/standards/wfl_girls_z_exp.txt

In our gulpfile, we'll transform them into some JSON that we can more
easily handle. We then store the JSON in three places:

- In our Javascript `assets` directory, so the frontend can ask for it.
- In a z-score module for the backend, so the backend can use it.
- As an Elm module for testing, so our Elm unit tests can use it.
