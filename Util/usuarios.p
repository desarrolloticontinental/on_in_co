output to c:\tmp\use-alm.txt.
for each pf-g004 where aplic-id = 'alm' and codcia = 0,
    first integral._user where integral._user._userid = pf-g004.user-id:
    display PF-G004.User-Id integral._user._user-name PF-G004.Seguridad 
        with stream-io no-labels width 200.
end.
output close.
