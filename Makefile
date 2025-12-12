DAYS := 1 2 3 4 5 6 7 8 9 10 11 12

.PHONY: run

run:
	@for d in $(DAYS); do \
		dir=day$$d; \
		echo "== Day $$d =="; \
		if [ -f "$$dir/main.c" ]; then \
			cc -O2 -std=c11 -D_POSIX_C_SOURCE=200809L -o "$$dir/day$$d" "$$dir/main.c" -lm; \
		fi; \
		c_time=""; py_time=""; \
		if [ -x "$$dir/day$$d" ]; then \
			c_out=$$(cd "$$dir" && ./day$$d 2>/dev/null); \
			echo "$$c_out"; \
			c_time=$$(printf "%s" "$$c_out" | sed -n 's/.*elapsed_ms=\([0-9.][0-9]*\).*/\1/p'); \
		else \
			echo "C binary not found for day $$d"; \
		fi; \
		req="$$dir/requirements.txt"; \
		if [ -f "$$req" ] && [ -s "$$req" ]; then \
			pip install -q -r "$$req"; \
		fi; \
		py_out=$$(cd "$$dir" && python main.py); \
		echo "$$py_out"; \
		py_time=$$(printf "%s" "$$py_out" | sed -n 's/.*elapsed_ms=\([0-9.][0-9]*\).*/\1/p'); \
		if [ -n "$$c_time" ] && [ -n "$$py_time" ]; then \
			diff_ms=$$(python -c "print(float('$$py_time')-float('$$c_time'))"); \
			echo "delta_ms (python - c): $$diff_ms"; \
		else \
			echo "delta_ms: N/A"; \
		fi; \
		echo ""; \
	done
