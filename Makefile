native :

% :
	@./make.sh $@


.PHONY: sv-comp-build sv-comp-smoketest-ubuntu sv-comp-smoketest-competition sv-comp-smoketest sv-comp

sv-comp-build:
	docker build . -f scripts/sv-comp/Dockerfile --output=scripts/sv-comp/

sv-comp-smoketest-ubuntu:
	docker build . -f scripts/sv-comp/Dockerfile --target smoketest-ubuntu

sv-comp-smoketest-competition:
	docker build . -f scripts/sv-comp/Dockerfile --target smoketest-competition

sv-comp-smoketest: sv-comp-smoketest-ubuntu sv-comp-smoketest-competition;
sv-comp: sv-comp-smoketest sv-comp-build;
