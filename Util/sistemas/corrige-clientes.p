DEF VAR x-direc LIKE gn-clie.dircli NO-UNDO.
DEF VAR x-nom LIKE gn-clie.nomcli NO-UNDO.

FOR EACH gn-clie NO-LOCK WHERE codcia = 0:
    RUN lib/limpiar-texto (nomcli, '', OUTPUT x-nom).
    RUN lib/limpiar-texto (dircli, '', OUTPUT x-direc).
    IF x-nom <> nomcli OR x-direc <> dircli THEN DO:
        DISPLAY codcli SKIP x-nom SKIP nomcli 
            SKIP x-direc SKIP dircli
            WITH STREAM-IO NO-BOX WIDTH 320.
    END.
END.
