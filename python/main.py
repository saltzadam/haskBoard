import sys
import yaml
import torch
import platform

from training_loop import train_multi_agent_on_policy
from ippo_instrumented import InstrumentedIPPO
from wrappers import DictSafeAsyncAgentsWrapper
from agilerl.algorithms.core.registry import HyperparameterConfig, RLParameter
from agilerl.hpo.mutation import Mutations
from agilerl.hpo.tournament import TournamentSelection
from agilerl.vector.pz_async_vec_env import AsyncPettingZooVecEnv

from haskboard_env import make

sys.stdout.reconfigure(line_buffering=True)  # type: ignore[union-attr]


def main(INIT_HP, MUTATION_PARAMS, NET_CONFIG, run_name: str = "default"):
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

    assert ((platform.system()[0:3].lower() == 'win') | (platform.system() == 'Linux'))

    if platform.system()[0:3].lower() == 'win':
        BINARY = r"C:\Users\HTPC\haskell\haskboard\dist-newstyle\build\x86_64-windows\ghc-9.10.1\NoMerci-0.1.0.0\x\NoMerci\build\NoMerci\NoMerci.exe"
    elif platform.system() == 'Linux':
        BINARY = "/home/adam/haskell/haskboard/dist-newstyle/build/x86_64-linux/ghc-9.10.1/NoMerci-0.1.0.0/x/NoMerci/build/NoMerci/NoMerci"


    num_envs = INIT_HP.get("NUM_ENVS", 1)
    env = AsyncPettingZooVecEnv(
        [lambda: make(BINARY, shared=False) for _ in range(num_envs)]
    )

    # Extract single (unbatched) spaces for agent construction
    obs_spaces = {a: env.single_observation_space(a) for a in env.possible_agents}
    act_spaces = {a: env.single_action_space(a) for a in env.possible_agents}

    INIT_HP["AGENT_IDS"] = env.possible_agents

    tournament = TournamentSelection(
        INIT_HP["TOURN_SIZE"],
        INIT_HP["ELITISM"],
        INIT_HP["POP_SIZE"],
        INIT_HP["EVAL_LOOP"],
    )

    mutations = Mutations(
        no_mutation=MUTATION_PARAMS["NO_MUT"],
        architecture=MUTATION_PARAMS["ARCH_MUT"],
        new_layer_prob=MUTATION_PARAMS["NEW_LAYER"],
        parameters=MUTATION_PARAMS["PARAMS_MUT"],
        activation=MUTATION_PARAMS["ACT_MUT"],
        rl_hp=MUTATION_PARAMS["RL_HP_MUT"],
        mutation_sd=MUTATION_PARAMS["MUT_SD"],
        rand_seed=MUTATION_PARAMS["RAND_SEED"],
        device=device,
    )

    hp_config = HyperparameterConfig(
        lr=RLParameter(min=MUTATION_PARAMS["MIN_LR"], max=MUTATION_PARAMS["MAX_LR"]),
        batch_size=RLParameter(
            min=MUTATION_PARAMS["MIN_BATCH_SIZE"],
            max=MUTATION_PARAMS["MAX_BATCH_SIZE"],
            dtype=int,
        ),
        learn_step=RLParameter(
            min=MUTATION_PARAMS["MIN_LEARN_STEP"],
            max=MUTATION_PARAMS["MAX_LEARN_STEP"],
            dtype=int,
            grow_factor=1.5,
            shrink_factor=0.75,
        ),
        ent_coef=RLParameter(
            min=MUTATION_PARAMS["MIN_ENT_COEF"],
            max=MUTATION_PARAMS["MAX_ENT_COEF"],
        ),
    )

    pop = [
        DictSafeAsyncAgentsWrapper(InstrumentedIPPO(
            observation_spaces=obs_spaces,
            action_spaces=act_spaces,
            agent_ids=INIT_HP["AGENT_IDS"],
            index=idx,
            hp_config=hp_config,
            net_config=NET_CONFIG,
            batch_size=INIT_HP.get("BATCH_SIZE", 64),
            lr=INIT_HP.get("LR", 0.0001),
            learn_step=INIT_HP.get("LEARN_STEP", 2048),
            gamma=INIT_HP.get("GAMMA", 0.99),
            gae_lambda=INIT_HP.get("GAE_LAMBDA", 0.95),
            action_std_init=INIT_HP.get("ACTION_STD_INIT", 0.0),
            clip_coef=INIT_HP.get("CLIP_COEF", 0.2),
            ent_coef=INIT_HP.get("ENT_COEF", 0.01),
            vf_coef=INIT_HP.get("VF_COEF", 0.5),
            max_grad_norm=INIT_HP.get("MAX_GRAD_NORM", 0.5),
            target_kl=INIT_HP.get("TARGET_KL"),
            update_epochs=INIT_HP.get("UPDATE_EPOCHS", 4),
            device=device,
            torch_compiler=INIT_HP["TORCH_COMPILE"],
        ))
        for idx in range(INIT_HP["POP_SIZE"])
    ]

    print("ready to train")

    train_multi_agent_on_policy(
        env,
        INIT_HP["ENV_NAME"],
        INIT_HP["ALGO"],
        pop,
        sum_scores=False,
        INIT_HP=INIT_HP,
        MUT_P=MUTATION_PARAMS,
        swap_channels=INIT_HP["CHANNELS_LAST"],
        max_steps=INIT_HP["MAX_STEPS"],
        evo_steps=INIT_HP["EVO_STEPS"],
        eval_steps=INIT_HP["EVAL_STEPS"],
        eval_loop=INIT_HP["EVAL_LOOP"],
        target=INIT_HP["TARGET_SCORE"],
        tournament=tournament,
        mutation=mutations,
        wb=INIT_HP["WANDB"],
        checkpoint_path="/home/adam/haskell/haskboard/python/runs/",
        checkpoint=1000,
        log_dir=f"/home/adam/haskell/haskboard/python/runs/{run_name}",
    )


if __name__ == "__main__":
    import argparse
    from datetime import datetime

    parser = argparse.ArgumentParser(description="Train IPPO on haskboard")
    parser.add_argument("--run", type=str, default=None,
                        help="Run name (used for log directory). Defaults to timestamp.")
    args = parser.parse_args()

    run_name = args.run or datetime.now().strftime("%Y%m%d_%H%M%S")
    print(f"Run: {run_name}")

    with open("ippo.yaml") as file:
        config = yaml.safe_load(file)
    main(config["INIT_HP"], config["MUTATION_PARAMS"], config["NET_CONFIG"],
         run_name=run_name)
