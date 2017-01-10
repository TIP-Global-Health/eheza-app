Feature: Check attendance functionality
  In order to be able to manage the attendance
  As authenticated user
  We need to be able to indicate the arrival of mothers to the clinic and to start the process

  Scenario Outline: Tap on a picture of an arrival mother
    Given I login as nurse
    When  I tap on "<customer>" image in the not yet arrived list
    Then  I should see the image moves to the here list

    Examples:
    | customer               |
    | Delphine Uwamahoro     |
    | Theophila Huriro Uwaca |
    | Wendy Leonard          |
    | Ann Doung              |

  Scenario: New mother arrives that is not on the list
    Given I login as nurse
    When  I see a mother that arrives and she is not listed
    Then  I tap on the add new mother icon
    And   I should see the add a mother page

  Scenario: Tap on the start button to start the health assessment
    Given I login as nurse
    When  I tap the start button
    Then  I should see the health assessment page
