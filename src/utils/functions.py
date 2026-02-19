import streamlit as st
import pandas as pd
from datetime import datetime
# --------------
# FUNCTIONS
# --------------
def logout():
    st.session_state.logged_in = False
    st.session_state.username = ""

def is_authed():
    return bool(st.session_state.owner_token and st.session_state.owner_username)

def project_loaded():
    return bool(
        st.session_state.selected_uid
        and st.session_state.survey_df is not None
        and st.session_state.choices_df is not None
        and st.session_state.data_file_bytes is not None
    )

def reset_project_state():
    st.session_state.selected_uid = None
    st.session_state.selected_asset_name = None
    st.session_state.survey_df = None
    st.session_state.choices_df = None
    st.session_state.data_export_url = None
    st.session_state.data_file_bytes = None
    st.session_state.label_colname = None
    reset_analysis_state()

def reset_analysis_state():
    # clear both tracked values + widget keys (to avoid sticky widgets)
    st.session_state.analysis_sheet = None
    st.session_state.analysis_variable = None
    st.session_state.analysis_disagg = None
    for k in ["analysis_sheet_widget", "analysis_variable_widget", "analysis_disagg_widget"]:
        if k in st.session_state:
            del st.session_state[k]

def format_time(time_string):
    if not time_string:
        return "—"
    time_string = datetime.fromisoformat(time_string.replace("Z", "+00:00"))
    return time_string.strftime("%Y-%m-%d %H:%M:%S")

def get_var_kind_and_listname(survey_df: pd.DataFrame, varname: str):
    """Return (kind, list_name). kind in {'select_one','select_multiple','numeric', None}"""
    if not varname or varname == "No disaggregation":
        return None, None

    # Try exact match
    row = survey_df.loc[survey_df["name"] == varname]

    # Fallback: if export column is "group/question", try matching "question"
    if row.empty and isinstance(varname, str) and "/" in varname:
        row = survey_df.loc[survey_df["name"] == varname.split("/")[-1]]

    if row.empty:
        return None, None

    t = str(row.iloc[0].get("type", "")).strip()

    if t.startswith("select_one"):
        parts = t.split(maxsplit=1)
        return "select_one", (parts[1].strip() if len(parts) == 2 else None)

    if t.startswith("select_multiple"):
        parts = t.split(maxsplit=1)
        return "select_multiple", (parts[1].strip() if len(parts) == 2 else None)

    if t in {"integer", "decimal"}:
        return "numeric", None

    return None, None

def build_label_map(choices_df: pd.DataFrame, list_name: str, label_col: str):
    """Map choice code -> label for a given list_name."""
    if not list_name:
        return {}
    df = choices_df[(choices_df["list_name"] == list_name) & (choices_df["name"].notna())].copy()
    if label_col not in df.columns:
        return {}
    return dict(zip(df["name"].astype(str), df[label_col].astype(str)))

def relabel_select_one_series(df_data: pd.DataFrame, survey_df: pd.DataFrame, choices_df: pd.DataFrame,
                              varname: str, label_col: str | None) -> pd.Series:
    kind, list_name = get_var_kind_and_listname(survey_df, varname)
    if kind != "select_one" or not list_name or not label_col:
        return df_data[varname]
    label_map = build_label_map(choices_df, list_name, label_col)
    return df_data[varname].astype(str).map(label_map).fillna(df_data[varname])

def available_label_languages(survey_df: pd.DataFrame):
    label_cols = [c for c in survey_df.columns if isinstance(c, str) and c.startswith("label::")]
    langs = [c.replace("label::", "") for c in label_cols]
    return label_cols, langs

def relabel_select_one_inplace(df: pd.DataFrame, survey_df: pd.DataFrame, choices_df: pd.DataFrame,
                               varname: str, label_col: str | None):
    if varname == "No disaggregation" or varname not in df.columns:
        return
    kind, list_name = get_var_kind_and_listname(survey_df, varname)
    if kind != "select_one" or not list_name or not label_col:
        return
    m = build_label_map(choices_df, list_name, label_col)
    df[varname] = df[varname].astype(str).map(m).fillna(df[varname])

def compute_graph_tidy(
    df_data: pd.DataFrame,
    survey_df: pd.DataFrame,
    choices_df: pd.DataFrame,
    variable: str,
    disaggregation: str,
    label_col: str | None,
    omit_na: bool = True
) -> tuple[pd.DataFrame, str]:
    """
    Returns (tidy_df, var_type)
    tidy_df columns depend on var_type:
      - select_one / select_multiple: ['category', 'value', 'disagg'(optional)]
      - numeric: ['metric','value'] (no disagg) OR ['disagg','metric','value'] (with disagg)
    """

    var_type, var_list = get_var_kind_and_listname(survey_df, variable)
    if var_type is None:
        return pd.DataFrame(), None

    no_disagg = (disaggregation == "No disaggregation")

    # Relabel select_one values for variable/disagg (like R does)
    if label_col:
        relabel_select_one_inplace(df_data, survey_df, choices_df, variable, label_col)
        if not no_disagg:
            relabel_select_one_inplace(df_data, survey_df, choices_df, disaggregation, label_col)

    # Optionally omit NA rows for the *main* variable
    if omit_na and variable in df_data.columns:
        df_data = df_data[~df_data[variable].isna()].copy()

    # ---- SELECT_ONE ----
    if var_type == "select_one":
        if no_disagg:
            s = df_data[variable].fillna("NA")
            counts = s.value_counts(dropna=False)
            total = counts.sum()
            out = pd.DataFrame({
                "category": counts.index.astype(str),
                "value": (counts / total * 100).astype(float)
            })
            return out, var_type
        else:
            # % within each disagg group
            ctab = pd.crosstab(df_data[disaggregation], df_data[variable], dropna=False)
            ctab = ctab.div(ctab.sum(axis=1), axis=0) * 100.0
            tidy = ctab.reset_index().melt(
                id_vars=[disaggregation],
                var_name="category",
                value_name="value"
            )
            tidy = tidy.rename(columns={disaggregation: "disagg"})
            # drop zeros to keep charts clean (like your R filter(value != 0))
            tidy = tidy[tidy["value"] != 0]
            return tidy, var_type

    # ---- SELECT_MULTIPLE ----
    if var_type == "select_multiple":
        # Kobo export for select_multiple is usually 0/1 columns like var/choice
        sm_cols_raw = [c for c in df_data.columns if str(c).startswith(variable + "/")]
        if not sm_cols_raw:
            return pd.DataFrame(), var_type

        # rename var/choice -> var___choice
        df_sm = df_data.copy()
        df_sm = df_sm.rename(columns=lambda c: str(c).replace("/", "___"))
        sm_cols = [str(c).replace("/", "___") for c in sm_cols_raw]
        df_sm[sm_cols] = df_sm[sm_cols].apply(pd.to_numeric, errors="coerce")

        # Relabel options (choice codes) using choices list if possible
        label_map = build_label_map(choices_df, var_list, label_col) if (label_col and var_list) else {}

        def opt_label(colname: str) -> str:
            # colname is "variable___option"
            opt = colname.split("___", 1)[1] if "___" in colname else colname
            return label_map.get(opt, opt)

        if no_disagg:
            # mean of 0/1 = proportion selected
            means = df_sm[sm_cols].mean(skipna=True) * 100.0
            out = pd.DataFrame({"category": [opt_label(c) for c in means.index], "value": means.values.astype(float)})
            out = out[out["value"] != 0]
            return out, var_type
        else:
            grouped = df_sm.groupby(df_data[disaggregation])[sm_cols].mean(skipna=True) * 100.0
            tidy = grouped.reset_index().melt(
                id_vars=[disaggregation],
                var_name="category",
                value_name="value"
            )
            tidy = tidy.rename(columns={disaggregation: "disagg"})
            tidy["category"] = tidy["category"].apply(opt_label)
            tidy = tidy[tidy["value"] != 0]
            return tidy, var_type

    # ---- NUMERIC ----
    if var_type == "numeric":
        df_num = df_data.copy()
        df_num[variable] = pd.to_numeric(df_num[variable], errors="coerce")

        if no_disagg:
            s = df_num[variable].dropna()
            if s.empty:
                return pd.DataFrame(), var_type
            out = pd.DataFrame({
                "metric": ["mean", "median", "min", "max"],
                "value": [float(s.mean()), float(s.median()), float(s.min()), float(s.max())]
            })
            return out, var_type
        else:
            # numeric by disagg: same 4 metrics
            g = df_num.groupby(df_num[disaggregation])[variable]
            res = g.agg(["mean", "median", "min", "max"]).reset_index()
            tidy = res.melt(id_vars=[disaggregation], var_name="metric", value_name="value")
            tidy = tidy.rename(columns={disaggregation: "disagg"})
            tidy = tidy.dropna(subset=["value"])
            return tidy, var_type

    return pd.DataFrame(), var_type

def graph_type_choices(var_type: str, has_disagg: bool, admin: str, disagg_is_numeric: bool):
    """
    Mirrors your R 'chooseGraph' logic but simplified to Plotly-friendly options.
    """
    if not has_disagg:
        if admin == "strata":
            return ["bar", "line"] if var_type != "numeric" else ["bar"]
        else:
            if var_type == "numeric":
                return ["bar", "gauge"]
            if var_type == "select_one":
                return ["bar", "line", "pie", "donut"]
            return ["bar", "line"]  # select_multiple
    else:
        if var_type == "numeric":
            return ["bar", "scatter"] if disagg_is_numeric else ["bar"]
        else:
            return ["bar", "heatmap"] if admin != "strata" else ["bar"]

