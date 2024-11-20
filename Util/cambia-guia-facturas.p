
for each ccbcdocu where
    codcia = 1 and
    coddoc = "fac" and
    nrodoc = "015102880":

    display
        coddoc
        NRODOC
        nomcli
        fchdoc
        imptot
        flgest.

    UPDATE
        flgest when coddoc = "G/R"
        NROREF FORMAT "X(20)"
        .
    /*
    for each ccbddocu of ccbcdoc :
        display ccbddocu.
    end.
    */
end.
