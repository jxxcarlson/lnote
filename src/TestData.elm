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
    , subject = "Example"
    , body = someText
    , tags = [ "foo", "bar" ]
    , timeCreated = Time.millisToPosix 1563424248000
    , timeModified = Time.millisToPosix 1563424248000
    }


someText =
    """
This is a test.  **I repeat:** a test.

[New York Times](http://nytimes.com)

<img src="https://3c1703fe8d.site.internapcdn.net/newman/csz/news/800/2018/divebombingf.jpg" style="width:300px;"  />

## Some Latin

orem ipsum dolor sit amet, tellus metus non convallis quis, bibendum tortor convallis, maecenas vel donec libero, dui donec nulla turpis. Commodo laoreet, amet sit vivamus mi. Neque diam a sed. Donec ut in dui, ipsum urna, magna commodo eu nulla, odio nonummy vel donec. Pede neque mauris imperdiet congue dictum, ultricies ipsum rutrum a arcu, scelerisque justo lectus lacus. Velit nulla nibh ante porttitor, volutpat congue turpis mauris nulla, in ligula nunc et ut lacus, vel congue turpis enim, ultricies vestibulum mauris cras arcu pulvinar.

Duis bibendum, fusce donec amet ipsum sagittis, luctus porta, a eu vitae convallis metus sodales imperdiet. Vitae erat, ultricies nec a, nulla interdum, donec ut risus, adipiscing wisi lorem pede leo arcu. Morbi semper tristique, luctus praesent massa, officiis nam sem ac. At nascetur sagittis pellentesque et posuere eget, tincidunt suspendisse morbi ac magna ligula suspendisse, a eros leo hac nonummy porta diam. Et proin integer, pulvinar velit mollis, nisl vestibulum curabitur. Habitasse est, sodales cras id integer placerat sit, ac erat ut tincidunt suspendisse dolor. In montes nibh pellentesque congue, faucibus donec id, natoque mattis dictum dictumst class massa hendrerit, arcu tristique in sed, lacus feugiat. Nec lorem, mi interdum aliquam magna mattis, varius morbi nulla et ipsum malesuada a, velit turpis pretium. Metus sed imperdiet arcu, eleifend integer non a, elit velit amet non nascetur massa, est et nibh purus.

A mus malesuada ut, nullam non hac sem integer aliquet, vitae ut, wisi harum semper sit vulputate duis, et ut pulvinar maecenas nostra vestibulum. Mi pellentesque, parturient ut, turpis tincidunt eu, erat dui est dui molestie. Rutrum ad mauris malesuada amet eros, metus neque nullam sociis elementum tempus mus, vel eu sit eleifend vitae. Eget mollis nulla. Magna enim suscipit, viverra tellus, wisi risus metus proin. Libero dolor id mattis quam vitae, ipsum in et tincidunt erat lorem donec, fermentum nec sed vestibulum. Arcu eget eget, at eleifend dui duis erat tellus, sodales vel metus. Sagittis lorem justo, malesuada vitae pulvinar. Quam massa suspendisse tristique vestibulum, class vehicula morbi, quas morbi nec metus dignissim ridiculus, dolor sed luctus lacinia. Ridiculus neque, justo mi rhoncus ipsum, vestibulum massa eget cursus adipiscing phasellus, sociis vitae integer ut, luctus pellentesque enim elit. Donec sed tempor lorem, libero sit, dolor magna duis, scelerisque magna ante vitae consectetuer congue wisi, dis vitae. Integer velit magna, urna leo et, luctus amet diam libero donec, enim ullamcorper rhoncus dolor ante, nulla malesuada venenatis neque facilisi adipiscing.

Malesuada vel per interdum ut nulla mattis, aliquet amet risus malesuada massa ornare elit, erat in ad odio eos molestie, felis sed bibendum ac adipiscing amet, posuere duis orci per. Ipsum elit, et quam amet, suscipit sed nulla ipsum porta, montes velit urna ante tortor. Scelerisque elementum. Lacus nam. Nibh pellentesque, vel eget, suspendisse per montes vitae. Nisl in, dictum risus purus, nec tempor accumsan vitae sit vel. Pede porttitor.
"""


n2 : Note
n2 =
    { id = 2
    , subject = "BBB"
    , body = "Also a test"
    , tags = [ "foo" ]
    , timeCreated = Time.millisToPosix 1563454248000
    , timeModified = Time.millisToPosix 1563454248000
    }
