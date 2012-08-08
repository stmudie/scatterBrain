PRO AS__SaxsProfileAddons::UpdateProfileWidgets, wBase

    WIDGET_CONTROL, Widget_Info(wBase, FIND_BY_UNAME='TIMESTMP') $
                    , SET_VALUE=self.timestamp

    WIDGET_CONTROL, Widget_Info(wBase, FIND_BY_UNAME='PROFTIME') $
                    , SET_VALUE=self.time

    WIDGET_CONTROL, Widget_Info(wBase, FIND_BY_UNAME='IOCOUNTS') $
                    , SET_VALUE=self.iocnts

    WIDGET_CONTROL, Widget_Info(wBase, FIND_BY_UNAME='BSCOUNTS') $
                    , SET_VALUE=self.bscnts

    WIDGET_CONTROL, Widget_Info(wBase, FIND_BY_UNAME='TRANSMIS') $
                    , SET_VALUE=self.itsf/self.tnrm

    WIDGET_CONTROL, Widget_Info(wBase, FIND_BY_UNAME='SFTOTAL') $
                    , SET_VALUE= self.sf

END

PRO AS__SaxsProfileAddons__define

  void = {AS__SaxsProfileAddons, $
          INHERITS as_profiledata }

END