/* vstates.i - viewer-specific ADM states */

    WHEN 'update-begin':U THEN DO:   /* Somebody pressed the Update button */
        RUN dispatch('enable-fields':U).
        RUN new-state ('update':U).  /* Tell others (query, nav panel... */
    END.
    WHEN 'update-complete':U THEN DO:
        RUN new-state ('update-complete':U).  /* Tell others... */
    END.
    

