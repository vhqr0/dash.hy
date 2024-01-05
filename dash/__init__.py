import hy

hy.macros.require("dash.dash", None, assignments="ALL", prefix="")
hy.macros.require("dash.misc", None, assignments="ALL", prefix="")

from dash.dash import *
from dash.misc import *
