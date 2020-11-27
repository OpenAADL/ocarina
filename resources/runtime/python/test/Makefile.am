ifeq ($(PYTHON_TEST), yes)
TESTS = metrics test_api
fi


check-local: $(TEST)

metrics:
	 ./metrics.py -a rma.aadl -r rma.impl

test_api:
	./test.py
