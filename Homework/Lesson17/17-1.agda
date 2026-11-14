module 17-1 where

data Day : Set where
    monday    : Day
    tuesday   : Day
    wednesday : Day
    thursday  : Day
    friday    : Day
    saturday  : Day
    sunday    : Day

nextday : Day -> Day
nextday monday    = tuesday
nextday tuesday   = wednesday
nextday wednesday = thursday
nextday thursday  = friday
nextday friday    = saturday
nextday saturday  = sunday
nextday sunday    = monday