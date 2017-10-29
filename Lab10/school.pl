% Prolog solution for SCHOOL's OUT!
% Braden Hitchcock, CS 330, 3.8.2017

% declare the sets

teacher(ms_appleton).
teacher(ms_gross).
teacher(mr_knight).
teacher(mr_mcevoy).
teacher(ms_parnell).

subject(english).
subject(gym).
subject(history).
subject(math).
subject(science).

state(california).
state(florida).
state(maine).
state(oregon).
state(virginia).

activity(antiquing).
activity(camping).
activity(sightseeing).
activity(spelunking).
activity(water_skiing).


% let's solve this bad boy...

solve :-
    
    % make sure that all the subjects are different
    subject(AppletonSubject), subject(GrossSubject), subject(KnightSubject), subject(McEvoySubject),
    subject(ParnellSubject),
    all_different([AppletonSubject, GrossSubject, KnightSubject, McEvoySubject, ParnellSubject]),

    % make sure all of the states are different
    state(AppletonState), state(GrossState), state(KnightState), state(McEvoryState),
    state(ParnellState),
    all_different([AppletonState, GrossState, KnightState, McEvoryState, ParnellState]),

    % make sure all of the activities are different
    activity(AppletonActivity), activity(GrossActivity), activity(KnightActivity),
    activity(McEvoyActivity), activity(ParnellActivity),
    all_different([AppletonActivity, GrossActivity, KnightActivity, 
                        McEvoyActivity, ParnellActivity]),
    
    % each solution will come in quadruples, so let's set that up...
    Values = [
        [ms_appleton, AppletonSubject, AppletonState, AppletonActivity],
        [ms_gross, GrossSubject, GrossState, GrossActivity],
        [mr_knight, KnightSubject, KnightState, KnightActivity],
        [mr_mcevoy, McEvoySubject, McEvoyState, McEvoyActivity],
        [ms_parnell, ParnellSubject, ParnellState, ParnellActivity]
    ],


    % now let's get started on the rules...

    % 1. Ms. Gross teaches either math or science. If Ms. Gross is going antiquing, then she
    %    is going to Florida; otherwise, she is going to California
    ( ( member([ms_gross, math, _, _], Values);
        member([ms_gross, science, _, _], Values) ),
      ( member([ms_gross, _, florida, antiquing], Values);
        ( member([ms_gross, _, california, _], Values),
          \+ member([ms_gross, _, _, antiquing], Values) ) ) ),

    % 2. The science teacher (who is going water-skiing) is going to travel to either California
    %    or Florida. Mr. McEvoy (who is the history teacher) is going to either Maine or Oregon
    ( member([_, science, california, water_skiing], Values);
      member([_, science, florida, water_skiing], Values)),
    ( member([mr_mcevoy, history, maine, _], Values);
      member([mr_mcevoy, history, oregon, _], Values)),

    % 3. If the woman who is going to Virginia is the English teaher, then she is Ms. Appleton;
    %    otherwise, she is Ms. Parnell (who is going spelunking)
    member([ms_parnell, _, _, spelunking], Values),
    ( ( member([_, english, virginia, _], Values),
        member([ms_appleton, _, _, _], Values));
      ( \+ member([_, english, virginia, _], Values),
        member([ms_parnell, _, _, _], Values) ) ),

    % 4. The person who is going to Maine (who isn't the gym teacher) isn't the one who is
    %    going sightseeing
    \+ member([_, gym, maine, _], Values),
    \+ member([_, _, maine, sightseeing], Values),

    % 5. Ms. Gross isn't the woman who is going camping. One woman is going antiquing on her
    %    vacation
    \+ member([ms_gross, _, _, camping], Values),
    ( member([ms_appleton, _, _, antiquing], Values);
      member([ms_gross, _, _, antiquing], Values);
      member([ms_parnell, _, _, antiquing], Values) ),

    tell(ms_appleton, AppletonSubject, AppletonState, AppletonActivity),
    tell(ms_gross, GrossSubject, GrossState, GrossActivity),
    tell(mr_knight, KnightSubject, KnightState, KnightActivity),
    tell(mr_mcevoy, McEvoySubject, McEvoyState, McEvoyActivity),
    tell(ms_parnell, ParnellSubject, ParnellState, ParnellActivity).




% Succeeds if all elements of the argument list are bound and different.
% Fails if any elements are unbound or equal to some other element.
all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]).



% print the results to the screen
tell(Teacher,Subject,State,Activity) :-
    write(Teacher), write(', who teaches '), write(Subject), write(', is going '), write(Activity),
    write(' in '), write(State), write('.'), nl.
