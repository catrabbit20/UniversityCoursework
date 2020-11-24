course(cmput325).
course(cmput175).
course(cmput201).
course(cmput204).
course(cmput100).



prerequisite(cmput204, cmput325).
%prerequisite(cmput201, cmput325).
prerequisite(cmput175, cmput201).
prerequisite(cmput175, cmput204).
%prerequisite(cmput325, cmput204).
%prerequisite(cmput325, cmput325).


prerequisite(cmput100, cmput175).

%prerequisite(cmput325, cmput100).


