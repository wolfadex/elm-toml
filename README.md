# elm-toml

For parsing [TOML](https://toml.io/) files in Elm.

Currently supports v1.0.0 (latest at the time of publishing).

- 🧪 tested
- 🚧 in-progress
- 🔮 future work

Supported:
- 🚧 root parser
- 🧪 String value
- 🧪 Integer value
- 🧪 Float value
- 🧪 Boolean value
- 🔮 Offset Date-Time value
- 🔮 Local Date-Time value
- 🔮 Local Date value
- 🔮 Local Time value
- 🧪 Array value
- 🧪 Inline Table value
- 🧪 tables
- 🧪 array of tables

-----
# elm-rfc3339

Parses a String into an [RFC 3339](https://datatracker.ietf.org/doc/html/rfc3339) date time format.

Some examples of what these look like...

A local time:

    Rfc3339.parse "09:15:22" == TimeLocal { hour = 9, minute = 15, second = 22 }

A local date:

    Rfc3339

For differences with ISO 8601, I recommend checking out <https://ijmacd.github.io/rfc3339-iso8601/>
