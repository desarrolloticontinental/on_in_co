TRIGGER PROCEDURE FOR REPLICATION-DELETE OF vtamcanal.

    {rpl/reptrig.i
    &Table  = vtamcanal
    &Key    = "STRING(vtamcanal.codcia,'999') + vtamcanal.CanalVenta"
    &Prg    = r-vtamcanal
    &Event  = DELETE
    &FlgDB0 = TRUE
    &FlgDB1 = TRUE      
    &FlgDB2 = TRUE
    &FlgDB3 = TRUE
    &FlgDB4 = TRUE
    &FlgDB5 = TRUE
    &FlgDB6 = TRUE
    &FlgDB7 = TRUE
    &FlgDB8 = TRUE
    &FlgDB9 = TRUE
    &FlgDB10 = TRUE
    &FlgDB11 = TRUE
    &FlgDB12 = TRUE
    &FlgDB13 = TRUE
    &FlgDB14 = TRUE
    &FlgDB15 = TRUE
    &FlgDB16 = TRUE
    &FlgDB17 = YES
    &FlgDB18 = YES
    &FlgDB19 = YES
    &FlgDB20 = NO   /* UTILEX */
    &FlgDB30 = YES
    }

