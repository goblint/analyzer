native :

% :
	@./make.sh $@


.PHONY: sv-comp-tag sv-comp-build sv-comp-smoketest-ubuntu sv-comp-smoketest-competition sv-comp-smoketest sv-comp

sv-comp-tag:
	git tag -m "SV-COMP 2026" svcomp26

sv-comp-build: sv-comp-tag
	docker build . -f scripts/sv-comp/Dockerfile --output=scripts/sv-comp/

sv-comp-smoketest-ubuntu: sv-comp-tag
	docker build . -f scripts/sv-comp/Dockerfile --target smoketest-ubuntu

sv-comp-smoketest-competition: sv-comp-tag
	docker build . -f scripts/sv-comp/Dockerfile --target smoketest-competition

sv-comp-smoketest: sv-comp-smoketest-ubuntu sv-comp-smoketest-competition;
sv-comp: sv-comp-smoketest sv-comp-build;
