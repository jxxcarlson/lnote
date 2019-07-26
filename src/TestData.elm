module TestData exposing (n1, n2, passwordDict, userDict, userInfo1)

import Dict
import Note exposing (Note)
import Time
import User exposing (PasswordDict, UserDict, UserInfo)


passwordDict : PasswordDict
passwordDict =
    Dict.fromList
        [ ( "jxxcarlson", "!@oboLocol@!" )
        , ( "socrates", "!@citpeks@!" )
        ]


userDict : UserDict Note
userDict =
    Dict.fromList
        [ ( "jxxcarlson", userInfo1 )
        ]


userInfo1 : UserInfo Note
userInfo1 =
    { email = "jxxcarlson@gmail.com", admin = True, counter = 2, data = [ n1, n2 ] }


n1 : Note
n1 =
    { id = 1
    , subject = "AAA"
    , body = someText ++ moreText
    , tags = [ "foo", "bar" ]
    , timeCreated = Time.millisToPosix 1563424248000
    , timeModified = Time.millisToPosix 1563424248000
    }


someText =
    """
## AAA: a Test

[New York Times](http://nytimes.com)

This is a test. **I repeat:** this is a test. I repeat: this is a test. I repeat: this is a test. I repeat: this is a test.
I repeat: this is a test. I repeat: this is a test.
I repeat: this is a test. I repeat: this is a test.
I repeat: this is a test. I repeat: this is a test.
I repeat: this is a test. I repeat: this is a test.

"""


moreText =
    """


I repeat: this is a test. I repeat: this is a test.

I repeat: this is a test. I repeat: this is a test.

I repeat: this is a test. I repeat: this is a test.

I repeat: this is a test. I repeat: this is a test.

I repeat: this is a test. I repeat: this is a test.
"""


n2 : Note
n2 =
    { id = 1
    , subject = "BBB"
    , body = "Also a test"
    , tags = [ "foo" ]
    , timeCreated = Time.millisToPosix 1563454248000
    , timeModified = Time.millisToPosix 1563454248000
    }
