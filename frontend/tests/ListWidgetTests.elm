module ListWidgetTests exposing (..)

import Expect
import Test exposing (..)
import ListWidget

-- Msg_Add

add_to_empty : Test
add_to_empty = test 
  "Adding an element to an empty list widget makes a list widget that only contains that element"
  (\_ -> Expect.equal 
    [4]
    (ListWidget.update (ListWidget.Msg_Add 4) [])
  )

add_to_end : Test
add_to_end = test
  "Adding an element adds it to the end of the list"
  (\_ -> Expect.equal 
    [1, 2, 3, 4]
    (ListWidget.update (ListWidget.Msg_Add 4) [1, 2, 3])
  )

-- Msg_Remove

remove : Test
remove = test
  "Msg_Remove removes the element at the given index"
  (\_ -> Expect.equal 
    [1, 3]
    (ListWidget.update (ListWidget.Msg_Remove 1) [1, 2, 3])
  )

remove_first : Test
remove_first = test
  "Msg_Remove 0 removes the first element"
  (\_ -> Expect.equal 
    [2, 3]
    (ListWidget.update (ListWidget.Msg_Remove 0) [1, 2, 3])
  )

remove_last : Test
remove_last = test
  "Msg_Remove with the index of the last element removes the last element"
  (\_ -> Expect.equal 
    [1, 2]
    (ListWidget.update (ListWidget.Msg_Remove 2) [1, 2, 3])
  )

remove_invalid_size_of_list : Test
remove_invalid_size_of_list = test
  "Msg_Remove with a too large out of bounds index leaves the list unchanged (exactly size of the list)"
  (\_ -> Expect.equal 
    [1, 2, 3]
    (ListWidget.update (ListWidget.Msg_Remove 3) [1, 2, 3])
  )

remove_invalid_too_large : Test
remove_invalid_too_large = test
  "Msg_Remove with a too large out of bounds index leaves the list unchanged (very large index)"
  (\_ -> Expect.equal 
    [1, 2, 3]
    (ListWidget.update (ListWidget.Msg_Remove 10) [1, 2, 3])
  )

remove_invalid_negative : Test
remove_invalid_negative = test
  "Msg_Remove with a negative index leaves the list unchanged"
  (\_ -> Expect.equal 
    [1, 2, 3]
    (ListWidget.update (ListWidget.Msg_Remove -1) [1, 2, 3])
  )

-- Msg_Edit

edit : Test
edit = test
  "Msg_Edit replaces the element at the given index by another"
  (\_ -> Expect.equal 
    ["Iruña", "Bilbo", "Gasteiz"]
    (ListWidget.update (ListWidget.Msg_Edit 1 "Bilbo") ["Iruña", "Donosti", "Gasteiz"])
  )

edit_first : Test
edit_first = test
  "Msg_Edit 0 replaces the first element"
  (\_ -> Expect.equal 
    ["Bilbo", "Donosti", "Gasteiz"]
    (ListWidget.update (ListWidget.Msg_Edit 0 "Bilbo") ["Iruña", "Donosti", "Gasteiz"])
  )

edit_last : Test
edit_last = test
  "Msg_Edit with index of last element replaces the last element"
  (\_ -> Expect.equal 
    ["Iruña", "Donosti", "Bilbo"]
    (ListWidget.update (ListWidget.Msg_Edit 2 "Bilbo") ["Iruña", "Donosti", "Gasteiz"])
  )

edit_invalid_size_of_list : Test
edit_invalid_size_of_list = test
  "Msg_Edit with a too large index does nothing (Test index is exactly size of list)"
  (\_ -> Expect.equal 
    ["Iruña", "Donosti", "Gasteiz"]
    (ListWidget.update (ListWidget.Msg_Edit 3 "Bilbo") ["Iruña", "Donosti", "Gasteiz"])
  )

edit_invalid_too_large : Test
edit_invalid_too_large = test
  "Msg_Edit with a too large index does nothing (Test index is a very large index)"
  (\_ -> Expect.equal 
    ["Iruña", "Donosti", "Gasteiz"]
    (ListWidget.update (ListWidget.Msg_Edit 10 "Bilbo") ["Iruña", "Donosti", "Gasteiz"])
  )

edit_invalid_negative : Test
edit_invalid_negative = test
  "Msg_Edit with a negative index does nothing"
  (\_ -> Expect.equal 
    ["Iruña", "Donosti", "Gasteiz"]
    (ListWidget.update (ListWidget.Msg_Edit -1 "Bilbo") ["Iruña", "Donosti", "Gasteiz"])
  )
