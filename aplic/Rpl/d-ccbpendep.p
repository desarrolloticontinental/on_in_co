TRIGGER PROCEDURE FOR REPLICATION-DELETE OF ccbpendep.
/*
    /* De las tiendas a la base principal */
    {rpl/reptrig.i
    &Table  = ccbpendep
    &Key    =  "string(ccbpendep.codcia,'999') + string(ccbpendep.coddoc,'x(3)') + ~
        string(ccbpendep.coddiv, 'x(5)') + string(ccbpendep.codref,'x(3)') + ~
        string(ccbpendep.nroref,'x(15)')"
    &Prg    = r-ccbpendep
    &Event  = DELETE
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
