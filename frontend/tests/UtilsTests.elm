module UtilsTests exposing (..)

import Utils exposing (..)
import Expect
import Test exposing (..)

-- enumerate

enumerate_empty : Test
enumerate_empty = test
  "enumerate of the empty list returns the empty list."
  (\_ -> Expect.equal 
    []
    (enumerate [])
  )

enumerate_one : Test
enumerate_one = test
  "enumerate of a list with a single element returns a list with a pair containing the index 0 and that element"
  (\_ -> Expect.equal 
    [(0, "Iruña")]
    (enumerate ["Iruña"])
  )

enumerate_many : Test
enumerate_many = test
  "enumerate of a list with many elements returns a list of pairs of each of the element and its index"
  (\_ -> Expect.equal 
    [(0, "Iruña"), (1, "Bilbo"), (2, "Gasteiz"), (3, "Donosti")]
    (enumerate ["Iruña", "Bilbo", "Gasteiz", "Donosti"])
  )

-- add_if

add_if_true : Test
add_if_true = test
  "add_if True appends the element to the list"
  (\_ -> Expect.equal
    ([1, 2, 3, 4])
    (add_if True 4 [1, 2, 3])
  )

add_if_false : Test
add_if_false = test
  "add_if False leaves the list unchanged"
  (\_ -> Expect.equal
    [1, 2, 3]
    (add_if False 4 [1, 2, 3])
  )

-- remove_at

remove : Test
remove = test
  "Msg_Remove removes the element at the given index"
  (\_ -> Expect.equal 
    [1, 3]
    (remove_at 1 [1, 2, 3])
  )

remove_first : Test
remove_first = test
  "Msg_Remove 0 removes the first element"
  (\_ -> Expect.equal 
    [2, 3]
    (remove_at 0 [1, 2, 3])
  )

remove_last : Test
remove_last = test
  "Msg_Remove with the index of the last element removes the last element"
  (\_ -> Expect.equal 
    [1, 2]
    (remove_at 2 [1, 2, 3])
  )

remove_invalid_size_of_list : Test
remove_invalid_size_of_list = test
  "Msg_Remove with a too large out of bounds index leaves the list unchanged (exactly size of the list)"
  (\_ -> Expect.equal 
    [1, 2, 3]
    (remove_at 3 [1, 2, 3])
  )

remove_invalid_too_large : Test
remove_invalid_too_large = test
  "Msg_Remove with a too large out of bounds index leaves the list unchanged (very large index)"
  (\_ -> Expect.equal 
    [1, 2, 3]
    (remove_at 10 [1, 2, 3])
  )

remove_invalid_negative : Test
remove_invalid_negative = test
  "Msg_Remove with a negative index leaves the list unchanged"
  (\_ -> Expect.equal 
    [1, 2, 3]
    (remove_at -1 [1, 2, 3])
  )

-- replace_at

edit : Test
edit = test
  "Msg_Edit replaces the element at the given index by another"
  (\_ -> Expect.equal 
    ["Iruña", "Bilbo", "Gasteiz"]
    (replace_at 1 "Bilbo" ["Iruña", "Donosti", "Gasteiz"])
  )

edit_first : Test
edit_first = test
  "Msg_Edit 0 replaces the first element"
  (\_ -> Expect.equal 
    ["Bilbo", "Donosti", "Gasteiz"]
    (replace_at 0 "Bilbo" ["Iruña", "Donosti", "Gasteiz"])
  )

edit_last : Test
edit_last = test
  "Msg_Edit with index of last element replaces the last element"
  (\_ -> Expect.equal 
    ["Iruña", "Donosti", "Bilbo"]
    (replace_at 2 "Bilbo" ["Iruña", "Donosti", "Gasteiz"])
  )

edit_invalid_size_of_list : Test
edit_invalid_size_of_list = test
  "Msg_Edit with a too large index does nothing (Test index is exactly size of list)"
  (\_ -> Expect.equal 
    ["Iruña", "Donosti", "Gasteiz"]
    (replace_at 3 "Bilbo" ["Iruña", "Donosti", "Gasteiz"])
  )

edit_invalid_too_large : Test
edit_invalid_too_large = test
  "Msg_Edit with a too large index does nothing (Test index is a very large index)"
  (\_ -> Expect.equal 
    ["Iruña", "Donosti", "Gasteiz"]
    (replace_at 10 "Bilbo" ["Iruña", "Donosti", "Gasteiz"])
  )

edit_invalid_negative : Test
edit_invalid_negative = test
  "Msg_Edit with a negative index does nothing"
  (\_ -> Expect.equal 
    ["Iruña", "Donosti", "Gasteiz"]
    (replace_at -1 "Bilbo" ["Iruña", "Donosti", "Gasteiz"])
  )

-- toupper_first

toupper_first_empty : Test
toupper_first_empty = test
  "toupper_first of the empty string is the empty string"
  (\_ -> Expect.equal 
    ""
    (toupper_first "")
  )

toupper_first_text : Test
toupper_first_text = test
  "toupper_first of a string capitalizes the first letter"
  (\_ -> Expect.equal 
    "Hello world"
    (toupper_first "hello world")
  )

toupper_first_non_text : Test
toupper_first_non_text = test
  "toupper_first leaves the string unchanged if the first character is not a string"
  (\_ -> Expect.equal 
    "1 hello world"
    (toupper_first "1 hello world")
  )

-- missing_to_be_a_multiple_of

missing_to_be_a_multiple_of__same_number_returns_zero : Test
missing_to_be_a_multiple_of__same_number_returns_zero = test
  "A number is missing 0 to be a multiple of itself"
  (\_ -> Expect.equal
    0
    (missing_to_be_a_multiple_of 5 5)
  )

missing_to_be_a_multiple_of__divisor_greater_than_number : Test
missing_to_be_a_multiple_of__divisor_greater_than_number = test
  "When the divisor is greater than the number, missing_to_be_a_multiple_of returns number - divisor"
  (\_ -> Expect.equal
    (7 - 5)
    (missing_to_be_a_multiple_of 7 5)
  )

missing_to_be_a_multiple_of__number_is_divisible : Test
missing_to_be_a_multiple_of__number_is_divisible = test
  "When the number is divisible by the divisor, missing_to_be_a_multiple_of returns 0"
  (\_ -> Expect.equal
    0
    (missing_to_be_a_multiple_of 5 20)
  )

missing_to_be_a_multiple_of__number_is_bigger : Test
missing_to_be_a_multiple_of__number_is_bigger = test
  "When the number is bigger than the divisor and not divisible, missing_to_be_a_multiple_of returns the difference between the number and the smallest multiple of the divisor that is greater than the number"
  (\_ -> Expect.equal
    (5 * 4 - 18)
    (missing_to_be_a_multiple_of 5 18)
  )

-- chunk

chunk__size_zero : Test
chunk__size_zero = test
  "chunking a list in zero sized lists returns an empty list"
  (\_ -> Expect.equal
    []
    (chunk 0 ["a", "b", "c"])
  )

chunk__size_greater_than_list_length : Test
chunk__size_greater_than_list_length = test
  "Chunking by a size greater than the list returns a list that only contains the original list"
  (\_ -> Expect.equal
    [["a", "b", "c"]]
    (chunk 10 ["a", "b", "c"])
  )

chunk__size_smaller_than_list_length : Test
chunk__size_smaller_than_list_length = test
  "Chunking by a size smaller than the list returns a list of lists of the given size with the elements of the original list"
  (\_ -> Expect.equal
    [["a", "b"], ["c", "d"], ["e", "f"]]
    (chunk 2 ["a", "b", "c", "d", "e", "f"])
  )

chunk__remaining_elements : Test
chunk__remaining_elements = test
  "When the length of the list is not divisible by the given size, the last list is shorter"
  (\_ -> Expect.equal
    [["a", "b"], ["c", "d"], ["e", "f"], ["g"]]
    (chunk 2 ["a", "b", "c", "d", "e", "f", "g"])
  )

-- slide

adjacent__adjacent_of_empty_list_is_empty_list : Test
adjacent__adjacent_of_empty_list_is_empty_list = test
  ("adjacent of an empty list is an empty list")
  (\_ -> Expect.equal
    []
    (adjacent [])
  )

adjacent__adjacent_of_a_list_of_one_element_is_empty_list : Test
adjacent__adjacent_of_a_list_of_one_element_is_empty_list = test
  ("adjacent of a list with a single element is an empty list")
  (\_ -> Expect.equal
    []
    (adjacent [4])
  )

adjacent__adjacent_of_a_list_of_two_elements_is_a_list_with_single_pair : Test
adjacent__adjacent_of_a_list_of_two_elements_is_a_list_with_single_pair = test
  ("adjacent of a list with two elements is a list with a single pair containing both elements")
  (\_ -> Expect.equal
    [(4, 5)]
    (adjacent [4, 5])
  )

adjacent__list_with_many_elements : Test
adjacent__list_with_many_elements = test
  ("adjacent of a list with many elements returns a list of all adjacent pairs")
  (\_ -> Expect.equal
    [(1, 2), (2, 3), (3, 4), (4, 5), (5, 6)]
    (adjacent [1, 2, 3, 4, 5, 6])
  )

-- has_duplicates

has_duplicates__an_empty_list_never_has_duplicates : Test
has_duplicates__an_empty_list_never_has_duplicates = test
  ("An empty list does not have duplicates")
  (\_ -> Expect.equal
    False
    (has_duplicates [])
  )

has_duplicates__a_list_with_one_element_never_has_duplicates : Test
has_duplicates__a_list_with_one_element_never_has_duplicates = test
  ("A list with a single element does not have duplicates")
  (\_ -> Expect.equal
    False
    (has_duplicates [4])
  )

has_duplicates__a_list_with_two_elements_has_duplicates_if_both_are_equal__false_case : Test
has_duplicates__a_list_with_two_elements_has_duplicates_if_both_are_equal__false_case = test
  ("A list with two different elements does not have duplicates")
  (\_ -> Expect.equal
    False
    (has_duplicates [4, 5])
  )

has_duplicates__a_list_with_two_elements_has_duplicates_if_both_are_equal__true_case : Test
has_duplicates__a_list_with_two_elements_has_duplicates_if_both_are_equal__true_case = test
  ("A list with two equal elements has duplicates")
  (\_ -> Expect.equal
    True
    (has_duplicates [4, 4])
  )

has_duplicates__a_list_with_no_equal_elements_has_no_duplicates : Test
has_duplicates__a_list_with_no_equal_elements_has_no_duplicates = test
  ("A list with no equal elements does not have duplicates")
  (\_ -> Expect.equal
    False
    (has_duplicates [4, 2, 0, 5, 7, 3])
  )
  
has_duplicates__duplicates_do_not_need_to_be_adjacent : Test
has_duplicates__duplicates_do_not_need_to_be_adjacent = test
  ("Duplicates do not need to be adjacent")
  (\_ -> Expect.equal
    True
    (has_duplicates [4, 2, 0, 5, 7, 3, 4])
  )
  
  -- last

last__an_empty_list_has_no_last_element : Test
last__an_empty_list_has_no_last_element = test
  ("An empty list has no last element")
  (\_ -> Expect.equal
    Nothing
    (last [])
  )

last__last_of_one_element : Test
last__last_of_one_element = test
  ("The last element of a list with a single element is that single element")
  (\_ -> Expect.equal
    (Just 5)
    (last [ 5 ])
  )

last__last_of_several_elements : Test
last__last_of_several_elements = test
  ("The last element of a list with several elements is the last element in the list")
  (\_ -> Expect.equal
    (Just 8)
    (last [ 5, 6, 3, 8 ])
  )
