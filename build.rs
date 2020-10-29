use std::process::Command;
use std::path::PathBuf;
use std::env;
use std::ffi::{OsString, OsStr};
use bindgen;
use cc;

const EXTRA_SRCS: &[&str] = &[
    "src/lang.l",
    "src/lang.y",
    "src/parse_ctx.h",
    "src/parser.c",
    "src/parser.h",
    "src/ast.c",
    "src/ast.h",
];

fn oscat(s: impl Into<OsString>, e: impl AsRef<OsStr>) -> OsString {
    let mut s: OsString = s.into();
    s.push(e);
    return s;
}

fn main() {
    let out_env = env::var("OUT_DIR").unwrap();
    let out_path = PathBuf::from(out_env);

    let bison_h = out_path.join("lang.tab.h");
    let bison_c = out_path.join("lang.tab.c");
    let bison_stat = Command::new("bison")
        .arg("-d")
        .arg("-o").arg(bison_h)
        .arg("-o").arg(&bison_c)
        .arg("src/lang.y")
        .spawn()
        .expect("Could not spawn bison!")
        .wait()
        .expect("Error running bison!");
    assert!(bison_stat.success());

    let lex_h = out_path.join("lex.yy.h");
    let lex_c = out_path.join("lex.yy.c");
    let flex_stat = Command::new("flex")
        .arg(oscat("--header-file=", lex_h))
        .arg("-o").arg(&lex_c)
        .arg("src/lang.l")
        .spawn()
        .expect("Could not spawn flex!")
        .wait()
        .expect("Error running flex!");
    assert!(flex_stat.success());

    let parser_rs = out_path.join("parser.rs");
    bindgen::builder()
        .header("src/parser.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Failed to generate bindings!")
        .write_to_file(parser_rs)
        .expect("Failed to write bindings to file!");

    cc::Build::new()
        .file("src/parser.c")
        .file("src/ast.c")
        .file(bison_c)
        .file(lex_c)
        .include(out_path)
        .include("src/")
        .compile("parser");

    for src in EXTRA_SRCS {
        println!("cargo:rerun-if-changed={}", src);
    }
}