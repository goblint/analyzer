native :

% :
	@./make.sh $@


.PHONY: sv-comp-build sv-comp-smoketest-ubuntu sv-comp-smoketest sv-comp

sv-comp-build:
	docker build . -f scripts/sv-comp/Dockerfile --output=scripts/sv-comp/

sv-comp-smoketest-ubuntu:
	docker build . -f scripts/sv-comp/Dockerfile --target smoketest

sv-comp-smoketest: sv-comp-smoketest-ubuntu;
sv-comp: sv-comp-smoketest sv-comp-build;
