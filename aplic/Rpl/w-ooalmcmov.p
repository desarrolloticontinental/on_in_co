TRIGGER PROCEDURE FOR REPLICATION-WRITE OF ooalmcmov.

    {rpl/reptrig.i
    &Table  = ooalmcmov
    &Key    = "STRING(ooalmcmov.codcia,'999') + ~
    STRING(ooalmcmov.codalm,'x(5)') + ~
        STRING(ooalmcmov.tipmov,'x') + ~
        STRING(ooalmcmov.codmov,'99') + ~
        STRING(ooalmcmov.nroser,'999') + ~
        STRING(ooalmcmov.nrodoc,'999999999')"
    &Prg    = r-ooalmcmov
    &Event  = CREATE    /* OJO: Para todo es CREATE */
    &FlgDB0 = NO          /* Replicas: Sede Remota -> Sede Principal */
    &FlgDB1 = TRUE    /* Plaza Lima Norte 00501 */
    &FlgDB2 = TRUE    /* Surquillo 00023 */
    &FlgDB3 = TRUE    /* Chorrillos 00027 */
    &FlgDB4 = TRUE    /* San Borja 00502 */
    &FlgDB5 = TRUE    /* La Molina 00503 */
    &FlgDB6 = TRUE    /* Beneficiencia 00504 */
    &FlgDB7 = TRUE    /* Feria Plaza Norte 00505 */
    &FlgDB8 = TRUE    /* La Rambla 00507 */
    &FlgDB9 = TRUE    /* San Isidro 00508 */
    &FlgDB10 = TRUE   /* Chiclayo 00065 */
    &FlgDB11 = TRUE   /* Atocongo 00510 */
    &FlgDB12 = TRUE   /* Angamos 00511 */
    &FlgDB13 = TRUE   /* Salaverry 00512 */
    &FlgDB14 = TRUE   /* Centro Civico 00513 */
    &FlgDB15 = TRUE   /* Primavera 00514 */
    &FlgDB16 = TRUE
    &FlgDB17 = TRUE
    &FlgDB18 = TRUE   /* AREQUIPA */
    &FlgDB19 = TRUE   /* TRUJILLO */
    &FlgDB20 = TRUE   /* SERVIDOR CENTRAL */
    &FlgDB30 = TRUE   /* SERVIDOR ATE */
    }
    
