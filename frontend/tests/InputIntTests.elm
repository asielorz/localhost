module InputIntTests exposing (..)

import InputInt
import Test exposing (..)
import Expect

-- make

make_empty : Test
make_empty = test
  "make_empty makes a State with value 0 and empty string"
  (\_ -> Expect.equal 
    { value = 0, text = "" }
    InputInt.make_empty
  )

make_zero : Test
make_zero = test
  "make 0 makes a State with value 0 and string '0'"
  (\_ -> Expect.equal 
    { value = 0, text = "0" }
    (InputInt.make 0)
  )

make_positive : Test
make_positive = test
  "make 45 makes a State with value 45 and string '45'"
  (\_ -> Expect.equal 
    { value = 45, text = "45" }
    (InputInt.make 45)
  )

make_negative : Test
make_negative = test
  "make -45 makes a State with value -45 and string '-45'"
  (\_ -> Expect.equal 
    { value = -45, text = "-45" }
    (InputInt.make -45)
  )

-- update (valid inputs)

update_empty_string : Test
update_empty_string = test
  "Update with an empty string does not change the value"
  (\_ -> Expect.equal 
    { value = 5, text = "" }
    (InputInt.update (InputInt.Msg_Input "") (InputInt.make 5))
  )

update_zero : Test
update_zero = test
  "Update with a string that represents a number sets both the value and the text (zero)"
  (\_ -> Expect.equal 
    { value = 0, text = "0" }
    (InputInt.update (InputInt.Msg_Input "0") (InputInt.make 5))
  )

update_positive : Test
update_positive = test
  "Update with a string that represents a number sets both the value and the text (positive non zero)"
  (\_ -> Expect.equal 
    { value = 123456, text = "123456" }
    (InputInt.update (InputInt.Msg_Input "123456") (InputInt.make 5))
  )

update_negative : Test
update_negative = test
  "Update with a string that represents a number sets both the value and the text (negative)"
  (\_ -> Expect.equal 
    { value = -123456, text = "-123456" }
    (InputInt.update (InputInt.Msg_Input "-123456") (InputInt.make 5))
  )

-- update (invalid inputs)

update_invalid_random_text : Test
update_invalid_random_text = test
  "Update with a string that does not represent a number leaves the state unchanged (random text)"
  (\_ -> Expect.equal 
    (InputInt.make 5)
    (InputInt.update (InputInt.Msg_Input "Hello") (InputInt.make 5))
  )

update_number_then_letter : Test
update_number_then_letter = test
  "Update with a string that does not represent a number leaves the state unchanged (number then a letter)"
  (\_ -> Expect.equal 
    (InputInt.make 5)
    (InputInt.update (InputInt.Msg_Input "25m") (InputInt.make 5))
  )

update_number_then_space : Test
update_number_then_space = test
  "Update with a string that does not represent a number leaves the state unchanged (number then space)"
  (\_ -> Expect.equal 
    (InputInt.make 5)
    (InputInt.update (InputInt.Msg_Input "25 ") (InputInt.make 5))
  )

update_space_then_number : Test
update_space_then_number = test
  "Update with a string that does not represent a number leaves the state unchanged (space then number)"
  (\_ -> Expect.equal 
    (InputInt.make 5)
    (InputInt.update (InputInt.Msg_Input " 25") (InputInt.make 5))
  )

update_space_between_minus_and_number : Test
update_space_between_minus_and_number = test
  "Update with a string that does not represent a number leaves the state unchanged (space between minus and number)"
  (\_ -> Expect.equal 
    (InputInt.make 5)
    (InputInt.update (InputInt.Msg_Input "- 25") (InputInt.make 5))
  )

