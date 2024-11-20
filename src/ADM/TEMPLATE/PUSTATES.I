/* pustates.i - update panel-specific ADM states */

    /* If a link has been activated or deactivated, then:
           If there's no active Tableio-Target, disable the panel;
           Else if we're in 'Save' mode, enable the target's fields
           as we did for the objects active at initialization. */

    WHEN "link-changed":U THEN 
    DO:
        DEFINE VARIABLE t-t-link AS CHARACTER NO-UNDO INIT "":U.
        RUN get-link-handle IN adm-broker-hdl
            (INPUT THIS-PROCEDURE, 'TABLEIO-TARGET':U, OUTPUT t-t-link)
                NO-ERROR.
        IF t-t-link NE "":U THEN 
        DO:
            RUN set-buttons (adm-panel-state).   /* reset to prior state */
            IF panel-type = 'SAVE':U THEN 
            DO:
              RUN request-attribute IN adm-broker-hdl
                  (THIS-PROCEDURE, 'TABLEIO-TARGET':U, 'FIELDS-ENABLED':U).
              IF RETURN-VALUE NE "YES":U THEN
                  RUN notify('enable-fields, TABLEIO-TARGET':U).
            END.
        END.
        ELSE
          RUN set-buttons ('disable-all':U).
    END.

    WHEN "record-available":U THEN 
    DO:
       adm-panel-state = 'initial':U.
       RUN set-buttons (adm-panel-state).
        IF panel-type = 'SAVE':U THEN
        DO:
            RUN request-attribute IN adm-broker-hdl
              (THIS-PROCEDURE, 'TABLEIO-TARGET':U, 'FIELDS-ENABLED':U).
            IF RETURN-VALUE NE "YES":U THEN
              RUN notify( 'enable-fields, TABLEIO-TARGET':U).
        END.
    END.

    /* no-record-available means the current query is empty; Add is valid.
       no-external-record-available means a needed External Table record
       isn't present; no update actions at all are valid. */
    WHEN "no-record-available":U OR
    WHEN "no-external-record-available":U THEN 
    DO:
        IF p-state = "no-record-available":U THEN
            adm-panel-state = 'add-only':U.
        ELSE adm-panel-state = 'disable-all':U.
        RUN set-buttons (adm-panel-state).
        IF panel-type = 'SAVE':U THEN
           RUN notify ('disable-fields,TABLEIO-TARGET':U).
    END.

    WHEN "update":U THEN 
    DO:
        adm-panel-state = 'action-chosen':U.
        RUN set-buttons (adm-panel-state).
    END.

    WHEN "update-complete":U THEN 
    DO:
        RUN get-attribute IN THIS-PROCEDURE ('AddFunction':U).
        IF (RETURN-VALUE <> 'Multiple-Records':U) OR NOT add-active THEN DO:
          adm-panel-state = 'initial':U.
          RUN set-buttons (adm-panel-state).
          IF panel-type = "UPDATE":U OR panel-type = "UPDATE-TRANS":U THEN 
          DO:
              RUN dispatch ('apply-entry':U).  /* Get focus into panel first. */
              RUN notify ('disable-fields,TABLEIO-TARGET':U).
          END.
        END.
    END.
