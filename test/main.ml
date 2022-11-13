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
