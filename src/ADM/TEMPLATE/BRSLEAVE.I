/* brsleave.i - trigger code for ROW-LEAVE trigger of SmartBrowse*/
/* If the object selected is not a SmartPanel button 
   (which could be e.g. Cancel or Reset), then save any changes to the row. 
   Otherwise let the button take the appropriate action. */
DEFINE VARIABLE widget-enter  AS HANDLE NO-UNDO.
DEFINE VARIABLE widget-frame  AS HANDLE NO-UNDO.
DEFINE VARIABLE widget-parent AS HANDLE NO-UNDO.
DEFINE VARIABLE h-panel-char  AS CHARACTER NO-UNDO.
DEFINE VARIABLE h-panel-hdle  AS HANDLE NO-UNDO.

DEFINE VARIABLE panel-state   AS CHARACTER NO-UNDO.

  /* If the object has a valid frame attribute, see if it's a SmartPanel. */
  widget-enter = last-event:widget-enter.
  IF VALID-HANDLE(widget-enter) THEN widget-parent = widget-enter:PARENT.
  IF VALID-HANDLE(widget-parent) AND widget-parent:TYPE NE "BROWSE":U
    THEN widget-frame = widget-enter:FRAME.  /* Can't check FRAME on Brs flds */

  IF ((NOT VALID-HANDLE(widget-enter)) OR  /* Some events don't go to a widget*/
      (widget-parent:TYPE = "BROWSE":U AND last-event:FUNCTION <> "RETURN" ) OR /* Clicked elsewhere in the Browser*/
      (NOT VALID-HANDLE(widget-frame)) OR  /* Check parent Frame if present */
      (NOT CAN-DO(widget-frame:PRIVATE-DATA, "ADM-PANEL":U))) 
      /*SmartPanel?*/
  THEN DO:                                 /* If not a SmartPanel then do upd */
      IF adm-brs-in-update AND last-event:FUNCTION <> "RETURN" THEN    
      DO:
       /* @VM JM 28/10/97 TRADUCCION */
        MESSAGE 
        "Usted debe completar o cancelar su actualización antes de salir del registro."
            VIEW-AS ALERT-BOX WARNING.
        RETURN NO-APPLY.
      END.
      /* If they selected some other object or the LEAVE was initiated 
         from outside then check before continuing. Otherwise just save. 
         If they were adding a new record and didn't change any initial values,
         make sure that gets Saved as well. */

      IF {&BROWSE-NAME}:CURRENT-ROW-MODIFIED  OR
         (last-event:FUNCTION = "RETURN" AND widget-parent:TYPE = "BROWSE":U ) OR
        (adm-new-record AND BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS = 1) THEN
      DO:
        IF VALID-HANDLE (widget-parent) AND widget-parent:TYPE NE "BROWSE":U
        THEN DO:
          /* @VM JM 28/10/97 TRADUCCION */
          MESSAGE 
          "El Actual registro ha sido modificado. " SKIP
          "¿Desea actualizar estos cambios?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-save AS LOGICAL.
          IF l-save THEN
          DO:
             RUN dispatch('update-record':U).
             IF RETURN-VALUE = "ADM-ERROR":U THEN 
                 RETURN NO-APPLY.
             ELSE IF adm-adding-record AND NOT adm-brs-in-update
               THEN DO:
               /* @VM JM 28/10/97 GRABADO CON MULTIPLES REGISTROS */
               /* con quien esta enlazado */

               RUN GET-LINK-HANDLE IN adm-broker-hdl ( INPUT THIS-PROCEDURE,
                                                       INPUT "TABLEIO",
                                                       OUTPUT h-panel-char).
                ASSIGN h-panel-hdle = WIDGET-HANDLE( h-panel-char ) NO-ERROR.
                IF VALID-HANDLE( h-panel-hdle )
                THEN DO:
                  RUN get-attribute IN h-panel-hdle ('AddFunction':U).
                  IF (RETURN-VALUE = 'Multiple-Records':U) THEN DO:
                      RUN dispatch('add-record':U).
                      /*
                      MESSAGE "ANTES 1". 
                      widget-enter = {&BROWSE-NAME}:FIRST-COLUMN.
                      {&BROWSE-NAME}:CURRENT-COLUMN = widget-enter.
                      */
                  END.
                END.
             END.
          END.
          ELSE RUN dispatch ('cancel-record':U).
        END. 
        ELSE DO:                                       
          /*
          MESSAGE "BINGO2" adm-brs-in-update adm-adding-record PANEL-STATE.
          */
          RUN dispatch('update-record':U).
          IF RETURN-VALUE = "ADM-ERROR":U THEN 
              RETURN NO-APPLY.
          ELSE IF (adm-adding-record AND (NOT adm-brs-in-update)) THEN DO: 
              /* @VM JM 28/10/97 GRABADO CON MULTIPLES REGISTROS */
              /* con quien esta enlazado */
              RUN GET-LINK-HANDLE IN adm-broker-hdl ( INPUT THIS-PROCEDURE,
                                                      INPUT "TABLEIO",
                                                      OUTPUT h-panel-char).
              ASSIGN h-panel-hdle = WIDGET-HANDLE( h-panel-char ) NO-ERROR.
              IF VALID-HANDLE( h-panel-hdle )
              THEN DO:
                RUN get-attribute IN h-panel-hdle ('AddFunction':U).
                IF (RETURN-VALUE = 'Multiple-Records':U) THEN DO:
                    RUN dispatch('add-record':U). 
                    /*
                    MESSAGE "ANTES 2". 
                    widget-enter = {&BROWSE-NAME}:FIRST-COLUMN.
                    {&BROWSE-NAME}:CURRENT-COLUMN = widget-enter.
                    */
                END.
              END.
          END.
        END.
      END.
  END.
