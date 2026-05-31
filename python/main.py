import sys
import yaml
import torch

from agilerl.training.train_multi_agent_on_policy import train_multi_agent_on_policy
from agilerl.utils.utils import create_population
from agilerl.algorithms.core.registry import HyperparameterConfig, RLParameter
from agilerl.hpo.mutation import Mutations
from agilerl.hpo.tournament import TournamentSelection

from haskboard_env import make

sys.stdout.reconfigure(line_buffering=True)  # type: ignore[union-attr]


def main(INIT_HP, MUTATION_PARAMS, NET_CONFIG):
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

    env = make(
        "/home/adam/haskell/haskboard/dist-newstyle/build/x86_64-linux/ghc-9.10.1/NoMerci-0.1.0.0/x/NoMerci/build/NoMerci/NoMerci",
        shared=False,
    )
    env.reset()

    INIT_HP["AGENT_IDS"] = env.agents

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

    pop = create_population(
        algo=INIT_HP["ALGO"],
        observation_space=env.observation_spaces,
        action_space=env.action_spaces,
        net_config=NET_CONFIG,
        INIT_HP=INIT_HP,
        hp_config=hp_config,
        population_size=INIT_HP["POP_SIZE"],
        num_envs=1,
        device=device,
        torch_compiler=INIT_HP["TORCH_COMPILE"],
    )

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
    )


if __name__ == "__main__":
    with open("ippo.yaml") as file:
        config = yaml.safe_load(file)
    main(config["INIT_HP"], config["MUTATION_PARAMS"], config["NET_CONFIG"])
