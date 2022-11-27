run:
	cargo run

run-ui:
	cd ui && just serve-dev

db-cli:
	pgcli -h localhost -p 5432 -u blind_test -W blind_test -d blind_test

build-img:
	docker build -t blind:latest .
