interface RocUtils.Utils
    exposes [boolToStr]
    imports []

boolToStr : Bool -> Str
boolToStr = \val ->
    if val == Bool.true then
        "true"
    else
        "false"