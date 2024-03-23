import hy

hy.macros.require("dash.core", None, assignments="ALL", prefix="")
hy.macros.require("dash.misc", None, assignments="ALL", prefix="")

from dash.core import *
from dash.misc import *
