(** Test plan: all parts of the code that do not involve user input are tested
    by OUnit tests. This includes all modules located in src/. The modules that
    do not have their own distinct test file where tested through the interface
    of other modules. These OUnit Tests were developed using a glass box
    approach to achieve coverage rates as high as feasible. We used bisect to
    ensure test coverage accross these modules. The code that handles user input
    is located in bin/main.ml. This code has been thoroughly play tested since
    we cannot write unit tests for user input. Our play testing included trying
    illegal inputs and ensuring no malformed input led to a crash or other
    unwanted behavior. This approach to testing demonstrates the correctness of
    our system because all parts of the code that run without user input have
    been covered with unit tests that show their correctness, and all other
    parts of the code have been thoroughly tested manually with both correct
    inputs and malformed inputs. Our project passes all tests in our suite and
    our play testing.*)

open OUnit2
open Game
open PropertyTest
open PlayerTest
open MonopolyTest
open TileTest
open ChanceTest
open StateTest
open ChestTest

let state_tests = []

let test_suite =
  "Monopoly test suite"
  >::: List.flatten
         [
           PropertyTest.tests;
           PlayerTest.tests;
           MonopolyTest.tests;
           TileTest.tests;
           StateTest.tests;
           ChanceTest.tests;
           ChestTest.tests;
         ]

let _ = run_test_tt_main test_suite
