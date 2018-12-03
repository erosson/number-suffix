# erosson/number-suffixes

Format numbers with nice human-readable suffixes.

This is an Elm port of the useful parts of https://github.com/erosson/swarm-numberformat.

For example:

```elm
import NumberSuffix exposing (standardConfig)      

NumberSuffix.format standardConfig 1.0e6 --> "1.00 million"
NumberSuffix.format {standardConfig|getSuffix=NumberSuffix.suffixStandardShort} 1.0e6 --> "1.00M"
```
