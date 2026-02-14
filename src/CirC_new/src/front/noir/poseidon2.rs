//! Poseidon2 permutation constraint builder for the Noir frontend.
//!
//! Decomposes the Poseidon2 permutation (t=4, RF=8, RP=56) over BN254 into
//! CirC IR primitives (PfNaryOp::Add, PfNaryOp::Mul).

use crate::ir::term::*;
use circ_fields::FieldT;

/// Helper: convert a hex string to a field literal Term.
fn hex_to_pf(field: &FieldT, hex: &str) -> Term {
    let int = rug::Integer::from_str_radix(hex, 16).unwrap();
    pf_lit(field.new_v(int))
}

/// S-box: x^5 = (x*x) * (x*x) * x  (3 multiplications).
fn single_box(x: Term) -> Term {
    let s = term![Op::PfNaryOp(PfNaryOp::Mul); x.clone(), x.clone()]; // x^2
    term![Op::PfNaryOp(PfNaryOp::Mul); s.clone(), s, x] // x^4 * x = x^5
}

/// External (full-round) 4×4 matrix multiplication.
/// Matches the Barretenberg/Noir reference implementation exactly.
fn matrix_multiplication_4x4(input: &[Term; 4]) -> [Term; 4] {
    // t0 = input[0] + input[1]
    let t0 = term![Op::PfNaryOp(PfNaryOp::Add); input[0].clone(), input[1].clone()];
    // t1 = input[2] + input[3]
    let t1 = term![Op::PfNaryOp(PfNaryOp::Add); input[2].clone(), input[3].clone()];
    // t2 = 2*input[1] + t1 = 2B + C + D
    let two_b = term![Op::PfNaryOp(PfNaryOp::Add); input[1].clone(), input[1].clone()];
    let t2 = term![Op::PfNaryOp(PfNaryOp::Add); two_b, t1.clone()];
    // t3 = 2*input[3] + t0 = 2D + A + B
    let two_d = term![Op::PfNaryOp(PfNaryOp::Add); input[3].clone(), input[3].clone()];
    let t3 = term![Op::PfNaryOp(PfNaryOp::Add); two_d, t0.clone()];
    // t4 = 4*t1 + t3 = A + B + 4C + 6D
    let two_t1 = term![Op::PfNaryOp(PfNaryOp::Add); t1.clone(), t1];
    let four_t1 = term![Op::PfNaryOp(PfNaryOp::Add); two_t1.clone(), two_t1];
    let t4 = term![Op::PfNaryOp(PfNaryOp::Add); four_t1, t3.clone()];
    // t5 = 4*t0 + t2 = 4A + 6B + C + D
    let two_t0 = term![Op::PfNaryOp(PfNaryOp::Add); t0.clone(), t0];
    let four_t0 = term![Op::PfNaryOp(PfNaryOp::Add); two_t0.clone(), two_t0];
    let t5 = term![Op::PfNaryOp(PfNaryOp::Add); four_t0, t2.clone()];
    // t6 = t3 + t5 = 5A + 7B + C + 3D
    let t6 = term![Op::PfNaryOp(PfNaryOp::Add); t3, t5.clone()];
    // t7 = t2 + t4 = A + 3B + 5C + 7D
    let t7 = term![Op::PfNaryOp(PfNaryOp::Add); t2, t4.clone()];

    [t6, t5, t7, t4]
}

/// Internal (partial-round) matrix multiplication using diagonal values.
/// result[i] = input[i] * diag[i] + sum(input)
fn internal_m_multiplication(field: &FieldT, input: &[Term; 4]) -> [Term; 4] {
    let diag = internal_matrix_diagonal(field);

    let sum = term(
        Op::PfNaryOp(PfNaryOp::Add),
        vec![
            input[0].clone(),
            input[1].clone(),
            input[2].clone(),
            input[3].clone(),
        ],
    );

    std::array::from_fn(|i| {
        let scaled = term![Op::PfNaryOp(PfNaryOp::Mul); input[i].clone(), diag[i].clone()];
        term![Op::PfNaryOp(PfNaryOp::Add); scaled, sum.clone()]
    })
}

/// Internal matrix diagonal constants for BN254 Poseidon2.
fn internal_matrix_diagonal(field: &FieldT) -> [Term; 4] {
    [
        hex_to_pf(field, "10dc6e9c006ea38b04b1e03b4bd9490c0d03f98929ca1d7fb56821fd19d3b6e7"),
        hex_to_pf(field, "0c28145b6a44df3e0149b3d0a30b3bb599df9756d4dd9b84a86b38cfb45a740b"),
        hex_to_pf(field, "00544b8338791518b2c7645a50392798b21f75bb60e3596170067d00141cac15"),
        hex_to_pf(field, "222c01175718386f2e2e82eb122789e352e105a3b8fa852613bc534433ee428b"),
    ]
}

/// All 64 rounds × 4 round constants for BN254 Poseidon2.
fn round_constants(field: &FieldT) -> [[Term; 4]; 64] {
    let rc: [[&str; 4]; 64] = [
        // Round 0 (full)
        ["19b849f69450b06848da1d39bd5e4a4302bb86744edc26238b0878e269ed23e5",
         "265ddfe127dd51bd7239347b758f0a1320eb2cc7450acc1dad47f80c8dcf34d6",
         "199750ec472f1809e0f66a545e1e51624108ac845015c2aa3dfc36bab497d8aa",
         "157ff3fe65ac7208110f06a5f74302b14d743ea25067f0ffd032f787c7f1cdf8"],
        // Round 1 (full)
        ["2e49c43c4569dd9c5fd35ac45fca33f10b15c590692f8beefe18f4896ac94902",
         "0e35fb89981890520d4aef2b6d6506c3cb2f0b6973c24fa82731345ffa2d1f1e",
         "251ad47cb15c4f1105f109ae5e944f1ba9d9e7806d667ffec6fe723002e0b996",
         "13da07dc64d428369873e97160234641f8beb56fdd05e5f3563fa39d9c22df4e"],
        // Round 2 (full)
        ["0c009b84e650e6d23dc00c7dccef7483a553939689d350cd46e7b89055fd4738",
         "011f16b1c63a854f01992e3956f42d8b04eb650c6d535eb0203dec74befdca06",
         "0ed69e5e383a688f209d9a561daa79612f3f78d0467ad45485df07093f367549",
         "04dba94a7b0ce9e221acad41472b6bbe3aec507f5eb3d33f463672264c9f789b"],
        // Round 3 (full)
        ["0a3f2637d840f3a16eb094271c9d237b6036757d4bb50bf7ce732ff1d4fa28e8",
         "259a666f129eea198f8a1c502fdb38fa39b1f075569564b6e54a485d1182323f",
         "28bf7459c9b2f4c6d8e7d06a4ee3a47f7745d4271038e5157a32fdf7ede0d6a1",
         "0a1ca941f057037526ea200f489be8d4c37c85bbcce6a2aeec91bd6941432447"],
        // Rounds 4-59 (partial: only first constant is non-zero)
        ["0c6f8f958be0e93053d7fd4fc54512855535ed1539f051dcb43a26fd926361cf", "0", "0", "0"],
        ["123106a93cd17578d426e8128ac9d90aa9e8a00708e296e084dd57e69caaf811", "0", "0", "0"],
        ["26e1ba52ad9285d97dd3ab52f8e840085e8fa83ff1e8f1877b074867cd2dee75", "0", "0", "0"],
        ["1cb55cad7bd133de18a64c5c47b9c97cbe4d8b7bf9e095864471537e6a4ae2c5", "0", "0", "0"],
        ["1dcd73e46acd8f8e0e2c7ce04bde7f6d2a53043d5060a41c7143f08e6e9055d0", "0", "0", "0"],
        ["011003e32f6d9c66f5852f05474a4def0cda294a0eb4e9b9b12b9bb4512e5574", "0", "0", "0"],
        ["2b1e809ac1d10ab29ad5f20d03a57dfebadfe5903f58bafed7c508dd2287ae8c", "0", "0", "0"],
        ["2539de1785b735999fb4dac35ee17ed0ef995d05ab2fc5faeaa69ae87bcec0a5", "0", "0", "0"],
        ["0c246c5a2ef8ee0126497f222b3e0a0ef4e1c3d41c86d46e43982cb11d77951d", "0", "0", "0"],
        ["192089c4974f68e95408148f7c0632edbb09e6a6ad1a1c2f3f0305f5d03b527b", "0", "0", "0"],
        ["1eae0ad8ab68b2f06a0ee36eeb0d0c058529097d91096b756d8fdc2fb5a60d85", "0", "0", "0"],
        ["179190e5d0e22179e46f8282872abc88db6e2fdc0dee99e69768bd98c5d06bfb", "0", "0", "0"],
        ["29bb9e2c9076732576e9a81c7ac4b83214528f7db00f31bf6cafe794a9b3cd1c", "0", "0", "0"],
        ["225d394e42207599403efd0c2464a90d52652645882aac35b10e590e6e691e08", "0", "0", "0"],
        ["064760623c25c8cf753d238055b444532be13557451c087de09efd454b23fd59", "0", "0", "0"],
        ["10ba3a0e01df92e87f301c4b716d8a394d67f4bf42a75c10922910a78f6b5b87", "0", "0", "0"],
        ["0e070bf53f8451b24f9c6e96b0c2a801cb511bc0c242eb9d361b77693f21471c", "0", "0", "0"],
        ["1b94cd61b051b04dd39755ff93821a73ccd6cb11d2491d8aa7f921014de252fb", "0", "0", "0"],
        ["1d7cb39bafb8c744e148787a2e70230f9d4e917d5713bb050487b5aa7d74070b", "0", "0", "0"],
        ["2ec93189bd1ab4f69117d0fe980c80ff8785c2961829f701bb74ac1f303b17db", "0", "0", "0"],
        ["2db366bfdd36d277a692bb825b86275beac404a19ae07a9082ea46bd83517926", "0", "0", "0"],
        ["062100eb485db06269655cf186a68532985275428450359adc99cec6960711b8", "0", "0", "0"],
        ["0761d33c66614aaa570e7f1e8244ca1120243f92fa59e4f900c567bf41f5a59b", "0", "0", "0"],
        ["20fc411a114d13992c2705aa034e3f315d78608a0f7de4ccf7a72e494855ad0d", "0", "0", "0"],
        ["25b5c004a4bdfcb5add9ec4e9ab219ba102c67e8b3effb5fc3a30f317250bc5a", "0", "0", "0"],
        ["23b1822d278ed632a494e58f6df6f5ed038b186d8474155ad87e7dff62b37f4b", "0", "0", "0"],
        ["22734b4c5c3f9493606c4ba9012499bf0f14d13bfcfcccaa16102a29cc2f69e0", "0", "0", "0"],
        ["26c0c8fe09eb30b7e27a74dc33492347e5bdff409aa3610254413d3fad795ce5", "0", "0", "0"],
        ["070dd0ccb6bd7bbae88eac03fa1fbb26196be3083a809829bbd626df348ccad9", "0", "0", "0"],
        ["12b6595bdb329b6fb043ba78bb28c3bec2c0a6de46d8c5ad6067c4ebfd4250da", "0", "0", "0"],
        ["248d97d7f76283d63bec30e7a5876c11c06fca9b275c671c5e33d95bb7e8d729", "0", "0", "0"],
        ["1a306d439d463b0816fc6fd64cc939318b45eb759ddde4aa106d15d9bd9baaaa", "0", "0", "0"],
        ["28a8f8372e3c38daced7c00421cb4621f4f1b54ddc27821b0d62d3d6ec7c56cf", "0", "0", "0"],
        ["0094975717f9a8a8bb35152f24d43294071ce320c829f388bc852183e1e2ce7e", "0", "0", "0"],
        ["04d5ee4c3aa78f7d80fde60d716480d3593f74d4f653ae83f4103246db2e8d65", "0", "0", "0"],
        ["2a6cf5e9aa03d4336349ad6fb8ed2269c7bef54b8822cc76d08495c12efde187", "0", "0", "0"],
        ["2304d31eaab960ba9274da43e19ddeb7f792180808fd6e43baae48d7efcba3f3", "0", "0", "0"],
        ["03fd9ac865a4b2a6d5e7009785817249bff08a7e0726fcb4e1c11d39d199f0b0", "0", "0", "0"],
        ["00b7258ded52bbda2248404d55ee5044798afc3a209193073f7954d4d63b0b64", "0", "0", "0"],
        ["159f81ada0771799ec38fca2d4bf65ebb13d3a74f3298db36272c5ca65e92d9a", "0", "0", "0"],
        ["1ef90e67437fbc8550237a75bc28e3bb9000130ea25f0c5471e144cf4264431f", "0", "0", "0"],
        ["1e65f838515e5ff0196b49aa41a2d2568df739bc176b08ec95a79ed82932e30d", "0", "0", "0"],
        ["2b1b045def3a166cec6ce768d079ba74b18c844e570e1f826575c1068c94c33f", "0", "0", "0"],
        ["0832e5753ceb0ff6402543b1109229c165dc2d73bef715e3f1c6e07c168bb173", "0", "0", "0"],
        ["02f614e9cedfb3dc6b762ae0a37d41bab1b841c2e8b6451bc5a8e3c390b6ad16", "0", "0", "0"],
        ["0e2427d38bd46a60dd640b8e362cad967370ebb777bedff40f6a0be27e7ed705", "0", "0", "0"],
        ["0493630b7c670b6deb7c84d414e7ce79049f0ec098c3c7c50768bbe29214a53a", "0", "0", "0"],
        ["22ead100e8e482674decdab17066c5a26bb1515355d5461a3dc06cc85327cea9", "0", "0", "0"],
        ["25b3e56e655b42cdaae2626ed2554d48583f1ae35626d04de5084e0b6d2a6f16", "0", "0", "0"],
        ["1e32752ada8836ef5837a6cde8ff13dbb599c336349e4c584b4fdc0a0cf6f9d0", "0", "0", "0"],
        ["2fa2a871c15a387cc50f68f6f3c3455b23c00995f05078f672a9864074d412e5", "0", "0", "0"],
        ["2f569b8a9a4424c9278e1db7311e889f54ccbf10661bab7fcd18e7c7a7d83505", "0", "0", "0"],
        ["044cb455110a8fdd531ade530234c518a7df93f7332ffd2144165374b246b43d", "0", "0", "0"],
        ["227808de93906d5d420246157f2e42b191fe8c90adfe118178ddc723a5319025", "0", "0", "0"],
        ["02fcca2934e046bc623adead873579865d03781ae090ad4a8579d2e7a6800355", "0", "0", "0"],
        ["0ef915f0ac120b876abccceb344a1d36bad3f3c5ab91a8ddcbec2e060d8befac", "0", "0", "0"],
        // Rounds 60-63 (full)
        ["1797130f4b7a3e1777eb757bc6f287f6ab0fb85f6be63b09f3b16ef2b1405d38",
         "0a76225dc04170ae3306c85abab59e608c7f497c20156d4d36c668555decc6e5",
         "1fffb9ec1992d66ba1e77a7b93209af6f8fa76d48acb664796174b5326a31a5c",
         "25721c4fc15a3f2853b57c338fa538d85f8fbba6c6b9c6090611889b797b9c5f"],
        ["0c817fd42d5f7a41215e3d07ba197216adb4c3790705da95eb63b982bfcaf75a",
         "13abe3f5239915d39f7e13c2c24970b6df8cf86ce00a22002bc15866e52b5a96",
         "2106feea546224ea12ef7f39987a46c85c1bc3dc29bdbd7a92cd60acb4d391ce",
         "21ca859468a746b6aaa79474a37dab49f1ca5a28c748bc7157e1b3345bb0f959"],
        ["05ccd6255c1e6f0c5cf1f0df934194c62911d14d0321662a8f1a48999e34185b",
         "0f0e34a64b70a626e464d846674c4c8816c4fb267fe44fe6ea28678cb09490a4",
         "0558531a4e25470c6157794ca36d0e9647dbfcfe350d64838f5b1a8a2de0d4bf",
         "09d3dca9173ed2faceea125157683d18924cadad3f655a60b72f5864961f1455"],
        ["0328cbd54e8c0913493f866ed03d218bf23f92d68aaec48617d4c722e5bd4335",
         "2bf07216e2aff0a223a487b1a7094e07e79e7bcc9798c648ee3347dd5329d34b",
         "1daf345a58006b736499c583cb76c316d6f78ed6a6dffc82111e11a63fe412df",
         "176563472456aaa746b694c60e1823611ef39039b2edc7ff391e6f2293d2c404"],
    ];

    std::array::from_fn(|r| {
        std::array::from_fn(|i| hex_to_pf(field, rc[r][i]))
    })
}

/// Poseidon2 permutation over 4 BN254 field elements.
///
/// Implements the full permutation: initial linear layer, 4 full rounds,
/// 56 partial rounds, 4 full rounds.
pub fn poseidon2_permutation(field: &FieldT, inputs: &[Term]) -> Vec<Term> {
    assert_eq!(inputs.len(), 4, "Poseidon2 permutation requires exactly 4 inputs");

    let rc = round_constants(field);
    let mut state: [Term; 4] = [
        inputs[0].clone(),
        inputs[1].clone(),
        inputs[2].clone(),
        inputs[3].clone(),
    ];

    // Apply initial linear layer
    state = matrix_multiplication_4x4(&state);

    // First 4 full rounds (rounds 0-3)
    for r in 0..4 {
        // Add round constants
        for i in 0..4 {
            state[i] = term![Op::PfNaryOp(PfNaryOp::Add); state[i].clone(), rc[r][i].clone()];
        }
        // S-box on all elements
        for i in 0..4 {
            state[i] = single_box(state[i].clone());
        }
        // External matrix multiply
        state = matrix_multiplication_4x4(&state);
    }

    // 56 partial rounds (rounds 4-59)
    for r in 4..60 {
        // Add round constant to state[0] only
        state[0] = term![Op::PfNaryOp(PfNaryOp::Add); state[0].clone(), rc[r][0].clone()];
        // S-box on state[0] only
        state[0] = single_box(state[0].clone());
        // Internal matrix multiply
        state = internal_m_multiplication(field, &state);
    }

    // Last 4 full rounds (rounds 60-63)
    for r in 60..64 {
        // Add round constants
        for i in 0..4 {
            state[i] = term![Op::PfNaryOp(PfNaryOp::Add); state[i].clone(), rc[r][i].clone()];
        }
        // S-box on all elements
        for i in 0..4 {
            state[i] = single_box(state[i].clone());
        }
        // External matrix multiply
        state = matrix_multiplication_4x4(&state);
    }

    state.to_vec()
}
