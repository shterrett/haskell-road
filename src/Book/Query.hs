module Book.Query

where

import Book.DB

character = [ x       | ["character",x] <- db ]
movie     = [ x       | ["movie",x]     <- db ]
actor     = [ x       | ["actor",x]     <- db ]
director  = [ x       | ["director",x]  <- db ]
release   = [ (x,y)   | ["release",x,y] <- db ]
direct    = [ (x,y)   | ["direct",x,y]  <- db ]
play      = [ (x,y,z) | ["play",x,y,z]  <- db ]
