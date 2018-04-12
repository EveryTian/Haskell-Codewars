module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Earth seconds = seconds / 31557600
ageOn Mercury seconds = ageOn Earth seconds / 0.2408467
ageOn Venus seconds = ageOn Earth seconds / 0.61519726
ageOn Mars seconds = ageOn Earth seconds / 1.8808158
ageOn Jupiter seconds = ageOn Earth seconds / 11.862615
ageOn Saturn seconds = ageOn Earth seconds / 29.447498
ageOn Uranus seconds = ageOn Earth seconds / 84.016846
ageOn Neptune seconds = ageOn Earth seconds / 164.79132
