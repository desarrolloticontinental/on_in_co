TRIGGER PROCEDURE FOR REPLICATION-WRITE OF ccbcierr.
/*
    {rpl/reptrig.i
    &Table  = ccbcierr
    &Key    = "string(ccbcierr.codcia,'999') + string(ccbcierr.usuario,'x(15)') + string(ccbcierr.fchcie,'99/99/99') + string(ccbcierr.horcie, 'x(5)')"
    &Prg    = r-ccbcierr
    &Event  = WRITE
    &FlgDB0 = NO
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
    &FlgDB17 = TRUE
    &FlgDB18 = TRUE
    &FlgDB19 = TRUE
    &FlgDB20 = TRUE
    &FlgDB30 = TRUE   /* SERVIDOR ATE */
    }
*/