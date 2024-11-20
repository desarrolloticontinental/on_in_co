/* GENERACION DE ARCHIVOS DE ENVIO A LIMA */

/* VENTAS */
OUTPUT TO c:\tmp\ccbcdocu.d.
FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.fchdoc < TODAY:
    EXPORT ccbcdocu.
END.
OUTPUT CLOSE.
OUTPUT TO c:\tmp\ccbddocu.d.
FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.fchdoc < TODAY,
    EACH ccbddocu OF ccbcdocu NO-LOCK:
    EXPORT ccbddocu.
END.
OUTPUT CLOSE.
OUTPUT TO c:\tmp\gn-clie.d.
FOR EACH gn-clie NO-LOCK:
    EXPORT gn-clie.
END.
OUTPUT CLOSE.
OUTPUT TO c:\tmp\gn-card.d.
FOR EACH gn-card NO-LOCK:
    EXPORT gn-card.
END.
OUTPUT CLOSE.

/* ALMACENES */
OUTPUT TO c:\tmp\almcmov.d.
FOR EACH almcmov NO-LOCK WHERE almcmov.fchdoc < TODAY
    AND NOT (almcmov.codmov = 03 OR almcmov.codmov = 33):
    EXPORT almcmov.
END.
OUTPUT CLOSE.
OUTPUT TO c:\tmp\almdmov.d.
FOR EACH almdmov NO-LOCK WHERE almdmov.fchdoc < TODAY:
    EXPORT almdmov.
END.
OUTPUT CLOSE.


/* CAJA */
OUTPUT TO c:\tmp\ccbccaja.d.
FOR EACH ccbccaja NO-LOCK WHERE ccbccaja.fchdoc < TODAY:
    EXPORT ccbccaja.
END.
OUTPUT CLOSE.
OUTPUT TO c:\tmp\ccbdcaja.d.
FOR EACH ccbccaja NO-LOCK WHERE ccbccaja.fchdoc < TODAY,
    EACH ccbdcaja OF ccbccaja NO-LOCK:
    EXPORT ccbdcaja.
END.
OUTPUT CLOSE.
OUTPUT TO c:\tmp\ccbcmov.d.
FOR EACH ccbcmov NO-LOCK WHERE ccbcmov.fchdoc < TODAY:
    EXPORT ccbcmov.
END.
OUTPUT CLOSE.
OUTPUT TO c:\tmp\ccbdmov.d.
FOR EACH ccbdmov NO-LOCK WHERE ccbdmov.fchdoc < TODAY:
    EXPORT ccbdmov.
END.
OUTPUT CLOSE.

OUTPUT TO c:\tmp\ccbcmvto.d.
FOR EACH ccbcmvto NO-LOCK WHERE ccbcmvto.fchdoc < TODAY:
    EXPORT ccbcmvto.
END.
OUTPUT CLOSE.
OUTPUT TO c:\tmp\ccbdmvto.d.
FOR EACH ccbcmvto NO-LOCK WHERE ccbcmvto.fchdoc < TODAY,
    EACH ccbdmvto NO-LOCK WHERE ccbdmvto.codcia = ccbcmvto.codcia
    AND ccbdmvto.coddoc = ccbcmvto.coddoc
    AND ccbdmvto.nrodoc = ccbcmvto.nrodoc:
    EXPORT ccbdmvto.
END.
OUTPUT CLOSE.
OUTPUT TO c:\tmp\ccbcierr.d.
FOR EACH ccbcierr NO-LOCK WHERE ccbcierr.fchcie < TODAY:
    EXPORT ccbcierr.
END.
OUTPUT CLOSE.
OUTPUT TO c:\tmp\ccbdecl.d.
FOR EACH ccbdecl NO-LOCK WHERE ccbdecl.fchcie < TODAY:
    EXPORT ccbdecl.
END.
OUTPUT CLOSE.
OUTPUT TO c:\tmp\ccbpendep.d.
FOR EACH ccbpendep NO-LOCK WHERE ccbpendep.fchcie < TODAY:
    EXPORT ccbpendep.
END.
OUTPUT CLOSE.
/* OUTPUT TO c:\tmp\vtadtickets.d.                                     */
/* FOR EACH vtadtickets NO-LOCK WHERE DATE(VtaDTickets.Fecha) < TODAY: */
/*     EXPORT vtadtickets.                                             */
/* END.                                                                */
/* OUTPUT CLOSE.                                                       */
