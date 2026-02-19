import streamlit as st
import pandas as pd
from datetime import datetime
import requests
import time
from io import BytesIO
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
    st.session_state.export_created_at = None
    st.session_state.last_submission_at = None
    st.session_state.export_lang = None
    st.session_state.export_uid = None
    st.session_state.submission_count = None
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


def refresh_export_and_reload_project(headers_owner, uid: str, tool_url_final: str, *, poll_seconds: int = 12):
    """
    After (re)creating an export, refresh:
      - export metadata (url, lang, created, last submission)
      - download export bytes into session_state.data_file_bytes
      - (re)download tool and parse survey/choices (keeps variables in sync)
      - reset analysis state
    """
    # 1) Re-fetch exports (optionally poll a bit because export creation can be async)
    export = None
    t0 = time.time()
    while time.time() - t0 < poll_seconds:
        r = requests.get(f"{api_root()}/assets/{uid}/exports/?format=json", headers=headers_owner)
        if r.status_code != 200:
            break
        results = r.json().get("results", [])
        if results:
            export = results[0]
            # If lang is present, we can stop early
            if (export.get("data") or {}).get("lang") is not None:
                break
        time.sleep(1.0)

    if not export:
        st.session_state.data_export_url = None
        st.session_state.export_lang = None
        return False, "Could not refresh exports."

    # 2) Update export metadata in session
    st.session_state.export_created_at = export.get("date_created")
    st.session_state.last_submission_at = export.get("last_submission_time")
    st.session_state.data_export_url = export.get("result")
    st.session_state.export_lang = (export.get("data") or {}).get("lang")
    st.session_state.export_uid = export.get("uid")

    # 3) Validate lang
    if st.session_state.export_lang != "_xml":
        return False, f"Export lang is '{st.session_state.export_lang}'. Expected '_xml'."

    if not st.session_state.data_export_url:
        return False, "Export URL missing."

    # 4) Download export bytes
    data_file = requests.get(st.session_state.data_export_url, headers=headers_owner)
    if data_file.status_code != 200:
        return False, f"Failed to download export file ({data_file.status_code})."

    st.session_state.data_file_bytes = data_file.content

    # 5) Re-download tool + parse (so survey/choices always match the loaded asset)
    data_tool = requests.get(tool_url_final, headers=headers_owner)
    if data_tool.status_code != 200:
        return False, f"Failed to download tool file ({data_tool.status_code})."

    try:
        df_survey = pd.read_excel(BytesIO(data_tool.content), sheet_name="survey", dtype=str)
        df_choices = pd.read_excel(BytesIO(data_tool.content), sheet_name="choices", dtype=str)
        st.session_state.survey_df = df_survey
        st.session_state.choices_df = df_choices
    except Exception as e:
        return False, f"Failed to parse tool survey/choices: {e}"

    # 6) Reset analysis state so widgets re-bind to the new bytes + survey
    reset_analysis_state()
    return True, "Project export refreshed."

def create_xml_export(selected_uid: str, headers_owner: dict):
    export_payload = {
        "fields_from_all_versions": "true",
        "group_sep": "/",
        "hierarchy_in_labels": "true",
        "lang": "_xml",
        "multiple_select": "both",
        "type": "xls"
    }
    return requests.post(
        f"{api_root()}/assets/{selected_uid}/exports/?format=json",
        json=export_payload,
        headers=headers_owner
    )

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


def read_me_tab():
    # Optional: small styling for "title-message" vibe
    st.markdown(
        """
        <style>
        .title-message { font-weight: 700; }
        </style>
        """,
        unsafe_allow_html=True,
    )


    col1, col2 = st.columns(2, gap="large")

    with col1:
        st.markdown('<h3 class="title-message">Introduction</h3>', unsafe_allow_html=True)
        st.write(
            "This analysis tool will provide various tools and functionalities to analyse and interpret "
            "data across your different assessment and tools. It was built in a way to provide a "
            "user-friendly interface and a range of tools tailored to specific IMPACT needs."
        )
        st.divider()

        st.markdown('<h3 class="title-message">Characteristics</h3>', unsafe_allow_html=True)
        st.markdown(
            """
- *Data input:* The tool allows you to download your finalized/cleaned data and KoBo Tools (XLS format).
- *Analysis Options:* The tool provides various selection of analysis methods and techniques depending on the desired outcomes. These can include **single tabular analysis, visualizations, variances, and key highlight findings.**
- *Customization:* Users will be able to customize their output depending on different analysis parameters and settings according to their specific requirements. Some of these parameters are **adding weights, strata, and others.**
            """
        )
        st.divider()

        st.markdown('<h3 class="title-message">Parameters</h3>', unsafe_allow_html=True)

        st.markdown('#### **Singular Table Output**')
        st.markdown(
            """
- ***Variable:*** Variable to analyse.
- ***Disaggregated variable:*** Disaggregated variable against the first chosen variable.
- ***Weighting:*** [Yes] or [No] to include weighting to data.
- ***Include NA:*** [Selected] will add all the NA values that are originally omitted.
- ***Overall:*** [Selected] will consider the calculations to the overall population.
- ***Strata:*** [Selected] will consider the calculations by different popluation groups (Strata).
- ***Minimum sample number:*** Minimum number of respondents per group analyzed, where the results can be considered significant.
            """
        )

        st.markdown('#### **Singular Graph Output**')
        st.markdown(
            """
- ***Variable:*** Variable to analyse.
- ***Disaggregated variable:*** Disaggregated variable against the first chosen variable.
- ***Weighting:*** [Yes] or [No] to include weighting to data.
- ***Overall:*** [Selected] will consider the calculations to the overall population.
- ***Strata:*** [Selected] will consider the calculations by different popluation groups (Strata).
            """
        )

        st.markdown('<h3 class="title-message">Methodology</h3>', unsafe_allow_html=True)
        st.markdown('#### **Input Data**')
        st.write(
            "To be able to use all the functionalities of this tool, you should upload the cleaned data, "
            "the Kobo tool, and select the country related to the assessment "
            "(This is a required selection for the Map Output tab to start processing)."
        )

    with col2:
        st.markdown('#### **Singular Table/Graph Output**')
        st.write(
            "First, you will be able to select the respective sheet from your dataset "
            "(main sheet or other loops). Second, the other dropdown lists are the targeted variable for "
            "the analysis from the selected sheet as well as the disaggregated variabled to be added "
            "towards the targeted variable."
        )
        st.write(
            "The *variable and disaggregation* are mainly the select_one, select_multiple, and different "
            "numeric columns from your dataset. In case some of these columns are **empty** in your data, "
            "it will not be shown in the dropdown list."
        )
        st.write(
            "The output is an interactive table/graph that will be updated automatically depending on "
            "your different selections in the Parameters section on the left or even the variable or "
            "disagregation."
        )
        st.write(
            "For the tabular tab, if the selected variable is a select_one or select_multiple, the table "
            "will output percentages of the different categories of the selected variable. If the selected "
            "variable is numeric, then the table will mainly show the mean, median, min, and max values "
            "in the data."
        )

        st.markdown('<h3 class="title-message">Requirements</h3>', unsafe_allow_html=True)
        st.markdown('#### **Singular Table/Graph Output**')
        st.write("The Data downloaded should be of an Excel format. The dataset should include the all the sheets that usually are included once downloaded from Kobo Server.")
        st.write("The tool should include the survey and the choices tab.")
        st.write(
            'For the weighting to be captured and calculated, the data should include a column named **weight.** '
            "Please make sure that in case data is consiting of loops as well, weight column should also be included in every tab."
        )
        st.write(
            'For the strata to be captured and calculated, the data should include a column named **strata.** '
            "Please make sure that in case data is consiting of loops as well, strata column should also be included in every tab."
        )

        st.divider()

        st.markdown('<h3 class="title-message">Credits</h3>', unsafe_allow_html=True)
        st.write("This application was built by Abraham Azar. For any information or question, please contact abraham.azar30@outlook.com")


def create_xml_export(selected_uid: str, headers_owner: dict):
    export_payload = {
        "fields_from_all_versions": "true",
        "group_sep": "/",
        "hierarchy_in_labels": "true",
        "lang": "_xml",
        "multiple_select": "both",
        "type": "xls"
    }
    r = requests.post(
        f"{api_root()}/assets/{selected_uid}/exports/?format=json",
        json=export_payload,
        headers=headers_owner
    )
    return r

# ----------------------------
# SMALL INTERNAL HELPERS
# ----------------------------
def api_root():
    return f"{st.session_state.kobo_url.rstrip('/')}/api/v2"


def headers_owner():
    return {"Authorization": f"Token {st.session_state.owner_token}"}


def tool_url_final_from_state():
    u = st.session_state.selected_tool_url
    if not isinstance(u, str) or not u:
        return None
    return u.replace("/?format=json", "")


def fetch_asset_details(uid: str) -> int:
    """Returns submission_count (int). Updates session_state.submission_count."""
    r = requests.get(f"{api_root()}/assets/{uid}/?format=json", headers=headers_owner())
    if r.status_code == 200:
        cnt = r.json().get("deployment__submission_count", 0)
        st.session_state.submission_count = int(cnt or 0)
        return st.session_state.submission_count
    # fallback: keep whatever we had
    st.session_state.submission_count = int(st.session_state.submission_count or 0)
    return st.session_state.submission_count


def fetch_latest_export_meta(uid: str):
    """
    Updates session_state export metadata fields:
      data_export_url, export_lang, export_created_at, last_submission_at, export_uid
    """
    r = requests.get(f"{api_root()}/assets/{uid}/exports/?format=json", headers=headers_owner())
    if r.status_code != 200:
        # keep previous, but ensure safe defaults
        return False

    results = r.json().get("results", [])
    if not results:
        st.session_state.data_export_url = None
        st.session_state.export_lang = None
        st.session_state.export_created_at = None
        st.session_state.last_submission_at = None
        st.session_state.export_uid = None
        return True

    exp = results[0]
    st.session_state.data_export_url = exp.get("result")
    st.session_state.export_lang = (exp.get("data") or {}).get("lang")
    st.session_state.export_created_at = exp.get("date_created")
    st.session_state.last_submission_at = exp.get("last_submission_time")
    st.session_state.export_uid = exp.get("uid")
    return True


def create_xml_export_fallback(uid: str) -> requests.Response:
    """
    Fallback if you don't have create_xml_export() in utils.
    If you DO have it, we won't call this.
    """
    payload = {
        "fields_from_all_versions": "true",
        "group_sep": "/",
        "hierarchy_in_labels": "true",
        "lang": "_xml",
        "multiple_select": "both",
        "type": "xls"
    }
    return requests.post(
        f"{api_root()}/assets/{uid}/exports/?format=json",
        headers=headers_owner(),
        json=payload
    )


def load_tool_and_export_bytes():
    """
    Requires:
      - selected_uid
      - selected_tool_url
      - data_export_url (session)
      - export_lang == '_xml'
    Commits:
      - survey_df, choices_df, data_file_bytes
    """
    tool_url_final = tool_url_final_from_state()
    if not tool_url_final:
        return False, "Tool download URL not found."

    if not st.session_state.data_export_url:
        return False, "Export URL missing."

    if st.session_state.export_lang != "_xml":
        return False, f"Export lang is '{st.session_state.export_lang}', expected '_xml'."

    t = requests.get(tool_url_final, headers=headers_owner())
    d = requests.get(st.session_state.data_export_url, headers=headers_owner())

    if t.status_code != 200:
        return False, f"Failed to download tool ({t.status_code})."
    if d.status_code != 200:
        return False, f"Failed to download export file ({d.status_code})."

    try:
        df_survey = pd.read_excel(BytesIO(t.content), sheet_name="survey", dtype=str)
        df_choices = pd.read_excel(BytesIO(t.content), sheet_name="choices", dtype=str)
    except Exception as e:
        return False, f"Failed to parse survey/choices: {e}"

    st.session_state.survey_df = df_survey
    st.session_state.choices_df = df_choices
    st.session_state.data_file_bytes = d.content

    # labels chosen later in Analysis
    st.session_state.label_colname = None

    # reset analysis widgets whenever we load/refresh data
    reset_analysis_state()
    return True, "Tool + export loaded."


def load_pipeline(uid: str, asset_name: str, tool_url: str, *, force_export_xml: bool = False) -> bool:
    """
    One single source of truth.
    - always refresh asset details + export metadata
    - optionally create a new _xml export
    - if we have a valid _xml export, download tool + export and commit
    """
    # commit selection early so status panel always knows what's selected
    st.session_state.selected_uid = uid
    st.session_state.selected_asset_name = asset_name
    st.session_state.selected_tool_url = tool_url

    # 1) asset details
    fetch_asset_details(uid)

    # 2) latest export meta
    ok = fetch_latest_export_meta(uid)
    if not ok:
        return False

    # 3) if user asked to force export OR export missing OR export not _xml -> create _xml export
    need_xml = (st.session_state.export_lang != "_xml")
    no_export = not bool(st.session_state.data_export_url)

    if force_export_xml or no_export or need_xml:
        # create export
        creator = globals().get("create_xml_export", None)
        if callable(creator):
            resp = creator(uid, headers_owner())
        else:
            resp = create_xml_export_fallback(uid)

        if resp.status_code != 201:
            st.error(f"Export creation failed: {resp.status_code} - {resp.text}")
            return False

        # refresh meta again AFTER creation
        fetch_latest_export_meta(uid)

    # 4) if export is good, load bytes+tool
    if st.session_state.export_lang == "_xml" and st.session_state.data_export_url:
        ok2, msg = load_tool_and_export_bytes()
        if not ok2:
            st.error(msg)
            return False

    return True


def project_status_panel():
    """Always show project/export metrics if a project is selected."""
    if not st.session_state.selected_uid:
        return

    st.markdown("### Project status")

    m1, m2, m3, m4, m5 = st.columns(5)
    with m1:
        st.metric("Asset", st.session_state.selected_asset_name or "—")
    with m2:
        st.metric("Submissions", str(st.session_state.submission_count or 0))
    with m3:
        st.metric("Export lang", st.session_state.export_lang or "—")
    with m4:
        st.metric("Export created", format_time(st.session_state.export_created_at) if st.session_state.export_created_at else "—")
    with m5:
        st.metric("Last submission", format_time(st.session_state.last_submission_at) if st.session_state.last_submission_at else "—")

    if st.session_state.export_lang and st.session_state.export_lang != "_xml":
        st.error(f"Export language is '{st.session_state.export_lang}'. Tool requires '_xml'.")
    elif st.session_state.export_lang == "_xml":
        st.success("Export language is _xml ✅")

    if st.session_state.last_submission_at and st.session_state.export_created_at:
        if st.session_state.last_submission_at > st.session_state.export_created_at:
            st.warning("There are submissions not included in the latest export.")
        else:
            st.info("Last submission is not newer than export, but you can still re-export (edits/changes may exist).")
