% Prolog solution for ROSIE'S ROSES
% Braden Hitchcock, CS 330, 3.8.2017

rose(cottage_beauty).
rose(golden_sunset).
rose(mountain_bloom).
rose(pink_paradise).
rose(sweet_dreams).

event(anniversary_party).
event(charity_auction).
event(retirement_banquet).
event(senior_prom).
event(wedding).

item(ballons).
item(candles).
item(chocolates).
item(place_cards).
item(streamers).

customer(hugh).
customer(ida).
customer(jeremy).
customer(leroy).
customer(stella).


solve :-
    
    % make sure that all of the roses are different
    rose(HughRose), rose(IdaRose), rose(JeremyRose), rose(LeroyRose), rose(StellaRose),
    all_different([HughRose, IdaRose, JeremyRose, LeroyRose, StellaRose]),

    % make sure that all of the events are different
    event(HughEvent), event(IdaEvent), event(JeremyEvent), event(LeroyEvent), event(StellaEvent),
    all_different([HughEvent, IdaEvent, JeremyEvent, LeroyEvent, StellaEvent]),

    % make sure that all of the items are different
    item(HughItem), item(IdaItem), item(JeremyItem), item(LeroyItem), item(StellaItem),
    all_different([HughItem, IdaItem, JeremyItem, LeroyItem, StellaItem]),

    % each solution is a quadruplet of values, so we need to specify them
    Values = [
        [hugh,HughRose,HughEvent,HughItem],
        [ida,IdaRose,IdaEvent,IdaItem],
        [jeremy,JeremyRose,JeremyEvent,JeremyItem],
        [leroy,LeroyRose,LeroyEvent,LeroyItem],
        [stella,StellaRose,StellaEvent,StellaItem]
    ],

    % now let's start applying what we know
    % 1. Jeremy made a purchase for the senior prom. Stella (sho didn't choose flowers
    %    for a wedding) picked the Cottage Beauty variety.
    member([jeremy, _, senior_prom, _], Values),
    member([stella, cottage_beauty, _, _], Values),
    \+ member([stella, _, wedding, _], Values),

    % 2. Hugh (who selected the Pink Paradise blooms) didn't choose flowers for either
    %    the charity auction or the wedding.
    member([hugh, pink_paradise, _, _], Values),
    \+ member([hugh, _, wedding, _], Values),
    \+ member([hugh, _, charity_auction, _], Values),
    
    % 3. The customer who picked roses for an anniversary party also bought streamers.
    %    The one shopping for a wedding chose the ballons.
    member([_, _, anniversary_party, streamers], Values),
    member([_, _, wedding, ballons], Values),

    % 4. The customer who bought the Sweet Dreams variety also bought some gourmet chocolates.
    %    Jeremy didn't pick the Mountain Bloom variety
    member([_, sweet_dreams, _, chocolates], Values),
    \+ member([jeremy, mountain_bloom, _, _], Values),

    % 5. Leroy was shopping for the retirement banquet. The customer in charge of decorating the
    %    senior prom also bought the candles
    member([leroy, _, retirement_banquet, _], Values),
    member([_, _, senior_prom, candles], Values),

    tell(hugh, HughRose, HughEvent, HughItem),
    tell(ida, IdaRose, IdaEvent, IdaItem),
    tell(jeremy, JeremyRose, JeremyEvent, JeremyItem),
    tell(leroy, LeroyRose, LeroyEvent, LeroyItem),
    tell(stella, StellaRose, StellaEvent, StellaItem).



% Succeeds if all elements of the argument list are bound and different.
% Fails if any elements are unbound or equal to some other element.
all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]). 



% Prints out the results of the program
tell(Customer,Rose,Event,Item) :-
    write(Customer), write(' bought the '), write(Rose), write(' and the '),
    write(Item), write(' for the '), write(Event), write('.'), nl.























