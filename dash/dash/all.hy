(import
  dash.dash.polyfill [__all__ :as polyfill-all]
  dash.dash.cons [__all__ :as cons-all]
  dash.dash.dash [__all__ :as dash-all])

(setv all [#* polyfill-all #* cons-all #* dash-all])
