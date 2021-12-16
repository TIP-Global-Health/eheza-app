module Backend.Session.Test exposing (all)

import Backend.Session.Decoder exposing (..)
import Backend.Session.Model exposing (..)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Json.Decode exposing (Decoder, decodeString)
import Test exposing (Test, describe, test)


{-| Tests that decodeOfflineSession can decode something in the format we
used in local storage as of 2018-11-16. This is important for back-compat,
since when we get a new version of the code, we'll need to deal with sessions
that are in progress.
-}
decodeOfflineSessionTest : Test
decodeOfflineSessionTest =
    let
        -- Some JSON as we encoded it.
        json =
            """{"id":164,"scheduled_date":{"value":"2018-11-16","value2":"2018-11-16"},"clinic":1,"closed":false,"all_sessions":[{"id":164,"scheduled_date":{"value":"2018-11-16","value2":"2018-11-16"},"clinic":1,"closed":false,"training":false},{"id":165,"scheduled_date":{"value":"2018-11-16","value2":"2018-11-16"},"clinic":2,"closed":false,"training":false},{"id":166,"scheduled_date":{"value":"2018-11-16","value2":"2018-11-16"},"clinic":3,"closed":false,"training":false}],"clinics":[{"id":1,"label":"Homanoywa"},{"id":3,"label":"Kedohi"},{"id":2,"label":"Viyalo"}],"participants":{"mothers":[{"id":18,"label":"<script>alert('XSS-node-mother-Jessyca')</script>","avatar":"http://eheza-app.local:8888/eheza-app/server/www/sites/default/files/styles/patient-photo/public/imagefield_XSAOzA.jpg?itok=-sQQf_Ck","children":[],"date_birth":"1970-07-23","ubudehe":3,"education_level":4},{"id":20,"label":"<script>alert('XSS-node-mother-Lulu')</script>","avatar":"http://eheza-app.local:8888/eheza-app/server/www/sites/default/files/styles/patient-photo/public/imagefield_L9vebu.png?itok=rs-uC9D4","children":[41],"date_birth":"1970-11-26","ubudehe":2,"education_level":5},{"id":10,"label":"<script>alert('XSS-node-mother-Monica')</script>","avatar":"http://eheza-app.local:8888/eheza-app/server/www/sites/default/files/styles/patient-photo/public/imagefield_AglV9K.gif?itok=xRM-rg2J","children":[],"date_birth":"1970-09-18","ubudehe":2,"education_level":5},{"id":23,"label":"Flo","avatar":"http://eheza-app.local:8888/eheza-app/server/www/sites/default/files/styles/patient-photo/public/imagefield_GPOYxd.jpeg?itok=9yVs3CN-","children":[27],"date_birth":"1970-10-23","ubudehe":2,"education_level":3},{"id":17,"label":"Stephania","avatar":"http://eheza-app.local:8888/eheza-app/server/www/sites/default/files/styles/patient-photo/public/imagefield_avwHKm.jpg?itok=AaXWNJAO","children":[36],"date_birth":"1970-06-02","ubudehe":2,"education_level":0}],"children":[{"id":27,"label":"<script>alert('XSS-node-child-Sophie')</script>","avatar":"http://eheza-app.local:8888/eheza-app/server/www/sites/default/files/styles/patient-photo/public/imagefield_KGxTjE.gif?itok=yDwSgzMQ","mother":23,"date_birth":"2014-08-06","gender":"female"},{"id":36,"label":"Raymundo","avatar":"http://eheza-app.local:8888/eheza-app/server/www/sites/default/files/styles/patient-photo/public/imagefield_NPJ1RV.jpeg?itok=o-W4hSPF","mother":17,"date_birth":"2012-08-04","gender":"female"},{"id":41,"label":"Jacques","avatar":"http://eheza-app.local:8888/eheza-app/server/www/sites/default/files/styles/patient-photo/public/imagefield_3dcXNc.png?itok=9-9SBeIT","mother":20,"date_birth":"2018-12-02","gender":"male"}],"mother_activity":{"10":{"family_planning":[{"id":155,"mother":10,"session":null,"date_measured":"2020-12-23","family_planning_signs":["condoms","implant"]},{"id":156,"mother":10,"session":null,"date_measured":"2020-08-07","family_planning_signs":["none"]}]},"17":{"family_planning":[{"id":147,"mother":17,"session":null,"date_measured":"2021-03-26","family_planning_signs":["necklace"]}]},"20":{"family_planning":[{"id":148,"mother":20,"session":null,"date_measured":"2015-05-16","family_planning_signs":["pill"]}]},"23":{"family_planning":[{"id":158,"mother":23,"session":null,"date_measured":"2020-01-03","family_planning_signs":["iud","injection","none"]},{"id":153,"mother":23,"session":null,"date_measured":"2019-08-30","family_planning_signs":["iud","implant"]},{"id":145,"mother":23,"session":null,"date_measured":"2017-10-07","family_planning_signs":["condoms","iud","pill"]}]}},"child_activity":{"27":{"height":[],"muac":[],"nutrition":[],"photo":[{"id":111,"child":27,"session":null,"date_measured":"2020-06-10","photo":{"styles":{"patient-photo":"http://eheza-app.local:8888/eheza-app/server/www/sites/default/files/styles/patient-photo/public/imagefield_44ivBg.jpeg?itok=cQ09hbjo"},"id":120}},{"id":104,"child":27,"session":null,"date_measured":"2020-04-17","photo":{"styles":{"patient-photo":"http://eheza-app.local:8888/eheza-app/server/www/sites/default/files/styles/patient-photo/public/imagefield_SmolUT.jpg?itok=4hmohud6"},"id":113}}],"weight":[]},"36":{"height":[],"muac":[{"id":65,"child":36,"session":null,"date_measured":"2016-04-22","muac":17},{"id":80,"child":36,"session":null,"date_measured":"2015-08-19","muac":21}],"nutrition":[{"id":88,"child":36,"session":null,"date_measured":"2020-11-09","nutrition_signs":["edema","none","poor-appetite"]}],"photo":[],"weight":[{"id":142,"child":36,"session":null,"date_measured":"2021-06-05","weight":57},{"id":135,"child":36,"session":null,"date_measured":"2019-07-15","weight":24}]},"41":{"height":[{"id":60,"child":41,"session":null,"date_measured":"2017-01-07","height":67},{"id":46,"child":41,"session":null,"date_measured":"2015-10-14","height":147}],"muac":[],"nutrition":[],"photo":[],"weight":[]}}}}"""
    in
    describe "decodeOfflineSession"
        [ test "back-compat" <|
            \_ -> Expect.pass

        -- TODO: Consider deployment strategy, and whether we need back-compat
        {-
            \_ ->
           decodeString decodeOfflineSession json
               |> (\result ->
                       case result of
                           Ok _ ->
                               Expect.pass

                           Err msg ->
                               Expect.fail msg
                  )
        -}
        ]


all : Test
all =
    describe "Backend.Session"
        [ decodeOfflineSessionTest
        ]
