# ScalaTropy

## How to run

Examples are provided in the `examples/` directory and use an MQTT-based network to demonstrate our language distributed capabilities.

- `PingPong`: a simple Ping-Pong example between two parties incrementing a counter for each message exchange;
- `TrianglePingPong`: a Ping-Pong example between three parties, where each party sends a message to the next one in a ring topology;
- `MasterWorker`: a Master-Worker application example where a master node distributes tasks to multiple worker nodes, waiting for their partial results to aggregate them into a final result.
- `matmul/*MatMulMasterWorker`: the Master-Worker example applied to the matrix multiplication problem. This example is provided in two versions, one using a broadcasting communication strategy and one using a selective communication strategy.

To run them, use the Docker Compose file provided in the root of the repository:

```bash
docker compose up
```

This will expose an MQTT broker on port `1883`.

You can then run the main methods provided in each example either by using your IDE or by following these steps:

```bash
# generate the JAR file for the example
./mill examples.assembly

# run
java -cp out/examples/assembly.dest/out.jar it.unibo.pslab.<MAIN_CLASS>
```

you need to replace `<MAIN_CLASS>` with the name of the main class of the example you want to run, e.g., `Pinger`.

## Evaluation experiments reproduction

The Message Overhead Analysis (c.f. Section 5.2 of the paper) can be reproduced by running the `RunExperiments.sc` script provided in the root of the repository.

```bash
./RunExperiments.sc
```

This will run the experiments for both the broadcasting and selective communication strategies, and will plot the results using the `plot_results.py` script, saving the resulting figure in the `evaluation/` directory together with the raw data.
