# Nested Proofs

In this tutorial we will verify the correctness of a zk-SNARK within another ZoKrates program.

This tutorial requires domain specific knowledge of ZoKrates.
We recommend that you read our [basic tutorials](https://zokratesplus.github.io/tutorials/) if you are new to zk-SNARKs or ZoKrates.
This tutorial assumes that you have installed ZoKrates on your local machine, please see the [installation guide](https://zokrates.github.io/gettingstarted.html#installation) if this is not the case.

## Compiling & Generating the Merkle Tree Proof
First we need to create a zk-SNARK proof with zokrates, the correctness of which will be verified later by another zokrates program.
For this tutorial we have chosen a Merkle root proof as a sample proof for our nested proof program.
The following instructions will help you to generate a valid Merkle Root proof, which will later be used as witness input in a subsequent proof.

1. Go to the proof subdirectory
   ```sh
   cd merkle_proof
   ```

2. Compile the Merkle root proof with the curve `bls12_377`. 
   ```sh
   $ zokrates compile -i merkle_proof.zok -o merkle_proof --curve bls12_377
   ```

    The output of the command should look something like this:
    ```sh
    Compiling merkle_proof.zok

    Compiled code written to merkle_proof
    Number of constraints: 169763
    ```

3. Run the setup with the `gm17` proving schema and `ark` as the backend
   ```sh
   $ zokrates setup --proving-scheme gm17 --backend ark -i merkle_proof
   ```

4. The file `merkle_proof/witness.json` contains the arguments of the generated valid merkle root. Use this file to generate a valid witness and then a valid proof: 
   ```sh
   $ zokrates compute-witness --abi -i merkle_proof --stdin < witness.json
   $ zokrates generate-proof --proving-scheme gm17 --backend ark -i merkle_proof
   ```

After successfully running all the previous commands, you will find the resulting SNARK in the `proof.json` file.

## Compile and generate the nested proof
Once we have a valid proof and the verification key from the Merkle root proof, we need to compile the nested proof and parse the arguments of `merkle_tree/proof.json` and `merkle_tree/verification.key` to generate a valid witness in the "parent" proof. 
To do this:

1. Back in the docker container, go back to the parent directory of this tutorial:
   ```sh
   $ cd ..
   ```

1. Compile & set up the nested proof with curve `bw6_761`, proving-scheme `gm17` and `ark` as backend
   ```sh
   $ zokrates compile --curve bw6_761 -i nested_proof.zok
   $ zokrates setup --proving-scheme gm17 --backend ark
   ```

1.  A sample of valid arguments for the nested proof is stored in `gm17.json'.

    Alternatively, you can compile the valid arguments by running the following command 
    ```sh
    echo "[\n$(cat merkle_proof/proof.json | jq '{proof, inputs}'), $(cat merkle_proof/verification.key | jq 'del(.scheme,.curve)')\n]" > jq > gm17.json
    ```

2. Compute a valid witness from the `gm17.json' file and generate the json proof.
To be found in `proof.json`.
   ```sh
   $ zokrates compute-witness --abi --stdin < gm17.json
   $ zokrates generate-proof --proving-scheme gm17 --backend ark
   ```

