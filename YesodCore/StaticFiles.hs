module YesodCore.StaticFiles where

import YesodCore.Settings     (appStaticDir, compileTimeAppSettings)
import Yesod.Static (staticFiles)

staticFiles (appStaticDir compileTimeAppSettings)
