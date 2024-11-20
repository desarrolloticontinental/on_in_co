/* bstates.i - browser-specific ADM states */
    WHEN "update-begin":U THEN
    DO:
        adm-brs-in-update = yes.
        RUN dispatch ('enable-fields':U).
        RUN dispatch ('apply-entry':U).
        RUN new-state('update':U).
    END.
    WHEN "update":U THEN 
        {&BROWSE-NAME}:SENSITIVE IN FRAME {&FRAME-NAME} = no.
    WHEN "update-complete":U THEN DO:
        {&BROWSE-NAME}:SENSITIVE IN FRAME {&FRAME-NAME} = yes.
        adm-brs-in-update = no.
    &IF DEFINED(TABLES-IN-QUERY-{&BROWSE-NAME}) <> 0 &THEN
        /* If this state message came from a query object (Query or Browser 
           with its own query) then we do *not* want to do row-changed, 
           because the row that was just updated was in our 
           Record-Target's query, not ours. */
        RUN get-attribute IN p-issuer-hdl ('QUERY-OBJECT':U).
        IF RETURN-VALUE NE "YES":U THEN
        DO:
          GET CURRENT {&BROWSE-NAME}. 
          RUN dispatch ('row-changed':U). 
        END.
    &ENDIF
        RUN new-state ('update-complete':U).  /* Pass on to others */
    END.
    WHEN "delete-complete":U THEN DO:
       DEFINE VARIABLE sts AS LOGICAL NO-UNDO.
       sts = {&BROWSE-NAME}:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME}.
    END.
