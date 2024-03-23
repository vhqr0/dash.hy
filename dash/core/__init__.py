import hy

hy.macros.require("dash.core.polyfill", None, assignments="ALL", prefix="")
hy.macros.require("dash.core.sequence", None, assignments="ALL", prefix="")
hy.macros.require("dash.core.colltools", None, assignments="ALL", prefix="")

from dash.core.polyfill import *
from dash.core.sequence import *
from dash.core.colltools import *
