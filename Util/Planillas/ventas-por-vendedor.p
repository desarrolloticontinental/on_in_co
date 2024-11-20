DEF BUFFER pedidos FOR faccpedi.
OUTPUT TO c:\tmp\rrhhcot.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'cot'
    AND fchped >= 03/01/2012
    AND fchped <= 12/31/2014
    AND codven = '379'
    AND flgest <> 'a',
    EACH pedidos NO-LOCK WHERE pedidos.codcia = 1
    AND pedidos.coddoc = 'ped'
    AND pedidos.codref = faccpedi.coddoc
    AND pedidos.nroref = faccpedi.nroped
    AND pedidos.flgest <> 'A',
    EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
    AND ccbcdocu.codped = pedidos.coddoc
    AND ccbcdocu.nroped = pedidos.nroped
    AND ccbcdocu.flgest <> 'A'
    AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0:
    PUT UNFORMATTED
        faccpedi.fchped '|'
        faccpedi.coddoc '|'
        faccpedi.nroped '|'
        faccpedi.codmon '|'
        faccpedi.imptot '|'
        ccbcdocu.coddoc '|'
        ccbcdocu.nrodoc '|'
        ccbcdocu.fchdoc '|'
        ccbcdocu.imptot
        SKIP.
END.
OUTPUT CLOSE.
/* OUTPUT TO c:\tmp\rrhhcot.txt.              */
/* FOR EACH faccpedi NO-LOCK WHERE codcia = 1 */
/*     AND coddoc = 'cot'                     */
/*     AND fchped >= 03/01/2012               */
/*     AND fchped <= 12/31/2014               */
/*     AND codven = '379'                     */
/*     AND flgest <> 'a',                     */
/*     EACH facdpedi OF faccpedi NO-LOCK:     */
/*     DISPLAY                                */
/*         faccpedi.fchped                    */
/*         faccpedi.coddoc                    */
/*         faccpedi.nroped                    */
/*         faccpedi.codmon                    */
/*         facdpedi.codmat                    */
/*         facdpedi.implin                    */
/*         WITH STREAM-IO NO-BOX WIDTH 320.   */
/* END.                                       */
/* OUTPUT CLOSE.                              */
/* OUTPUT TO c:\tmp\rrhhfac.txt.                       */
/* FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1 */
/*     AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL,TCK') > 0  */
/*     AND fchdoc >= 03/01/2012                        */
/*     AND fchdoc <= 12/31/2014                        */
/*     AND codven = '379'                              */
/*     AND flgest <> 'A',                              */
/*     EACH ccbddocu OF ccbcdocu NO-LOCK:              */
/*     DISPLAY                                         */
/*         ccbcdocu.fchdoc                             */
/*         ccbcdocu.coddoc                             */
/*         ccbcdocu.nrodoc                             */
/*         ccbcdocu.codmon                             */
/*         ccbddocu.codmat                             */
/*         ccbddocu.implin                             */
/*         WITH STREAM-IO NO-BOX WIDTH 320.            */
/* END.                                                */
/* OUTPUT CLOSE.                                       */
/* OUTPUT TO c:\tmp\rrhhndnc.txt.                      */
/* FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1 */
/*     AND LOOKUP(ccbcdocu.coddoc, 'N/C,N/D') > 0      */
/*     AND fchdoc >= 03/01/2012                        */
/*     AND fchdoc <= 12/31/2014                        */
/*     AND codven = '379'                              */
/*     AND flgest <> 'A':                              */
/*     DISPLAY                                         */
/*         ccbcdocu.fchdoc                             */
/*         ccbcdocu.coddoc                             */
/*         ccbcdocu.nrodoc                             */
/*         ccbcdocu.codmon                             */
/*         ccbcdocu.imptot                             */
/*         WITH STREAM-IO NO-BOX WIDTH 320.            */
/* END.                                                */
/* OUTPUT CLOSE.                                       */
/*                                                     */
