import streamlit as st
import pandas as pd
import requests
from PIL import Image
from io import BytesIO
from datetime import datetime
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
from src.utils.functions import *

# --------------
# APP SETTINGS
# --------------
st.set_page_config(page_title="Analysis App", layout="wide")

# --------------
# MANUAL CREDENTIALS (NOTE: move to st.secrets for real deployments)
# --------------
USER_CREDENTIALS = {
    "admin": {"password": "admin"},
    "drc_user": {"password": "kobo@drc@2O26"},
}

# --------------
# SESSION INIT
# --------------
if "logged_in" not in st.session_state:
    st.session_state.logged_in = False
    st.session_state.username = ""

# --------------
# SESSION DEFAULTS
# --------------
DEFAULT_KEYS = [
    # auth + server
    "kobo_url", "owner_token", "owner_username",

    # assets cache
    "df_assets",

    # selected/loaded project
    "selected_uid", "selected_asset_name",

    # loaded project data
    "survey_df", "choices_df", "data_export_url", "data_file_bytes",

    # analysis settings
    "label_colname",

    # analysis widget state (must reset when project changes)
    "analysis_sheet", "analysis_variable", "analysis_disagg",
    "analysis_sheet_widget", "analysis_variable_widget", "analysis_disagg_widget",
]
for k in DEFAULT_KEYS:
    if k not in st.session_state:
        st.session_state[k] = None

# --------------
# LOGIN FORM
# --------------
if not st.session_state.logged_in:
    logo = Image.open("src/images/logo_beginning.png")
    st.image(logo, width=120)
    st.markdown("<h2 style='text-align: center;'>ANALYSIS APP</h2>", unsafe_allow_html=True)
    st.markdown("### Login")

    with st.form("login_form"):
        username = st.text_input("Username")
        password = st.text_input("Password", type="password")
        submitted = st.form_submit_button("Login")

        if submitted:
            if username in USER_CREDENTIALS and USER_CREDENTIALS[username]["password"] == password:
                st.session_state.logged_in = True
                st.session_state.username = username
                st.success("Welcome!")
                st.rerun()
            else:
                st.error("Invalid username or password.")

# --------------
# MAIN APP AFTER LOGIN
# --------------
if st.session_state.logged_in:
    # --- SIDEBAR: connection/auth ---
    st.sidebar.markdown("## 🔌 Connection")
    st.session_state.kobo_url = st.sidebar.text_input(
        "Kobo server URL",
        value=st.session_state.kobo_url or "https://kobo.drc.ngo",
        help="Example: https://kobo.drc.ngo"
    ).rstrip("/")

    CONFIG = {"API_ROOT": f"{st.session_state.kobo_url}/api/v2"}

    st.sidebar.markdown("## 🔐 Authentication")
    token_input = st.sidebar.text_input(
        "Owner API Token",
        value="" if not st.session_state.owner_token else st.session_state.owner_token,
        type="password",
        placeholder="Paste token here"
    )

    colA, colB = st.sidebar.columns([1, 1])
    with colA:
        auth_btn = st.sidebar.button("Authenticate", use_container_width=True)
    with colB:
        if st.sidebar.button("Logout", use_container_width=True):
            logout()
            st.rerun()

    if auth_btn:
        headers_owner = {"Authorization": f"Token {token_input}"}
        with st.sidebar:
            with st.status("Authenticating…", expanded=False) as status:
                owner_resp = requests.get(
                    f"{CONFIG['API_ROOT']}/access-logs/me/?format=json&limit=1",
                    headers=headers_owner
                )
                if owner_resp.status_code == 200:
                    st.session_state.owner_token = token_input
                    st.session_state.owner_username = owner_resp.json()["results"][0]["username"]
                    status.update(label="Authenticated ✅", state="complete")
                else:
                    st.session_state.owner_token = None
                    st.session_state.owner_username = None
                    status.update(label="Authentication failed ❌", state="error")
                    reset_project_state()

        # Reset project data on auth changes (good hygiene)
        reset_project_state()
        st.rerun()

    if is_authed():
        st.sidebar.success(f"Signed in as: {st.session_state.owner_username}")
    else:
        st.sidebar.info("Authenticate to view projects.")

    st.sidebar.markdown("---")
    nav = st.sidebar.radio("Navigation", ["Read Me", "Project Selection", "Analysis"])

    # Gate Analysis page
    if nav == "Analysis" and not project_loaded():
        st.sidebar.warning("Select and load a project first.")
        nav = "Project Selection"

    # --------------
    # PAGES
    # --------------
    if nav == "Read Me":
        st.title("📘 Read Me")
        col1, col2 = st.columns([1, 1])

        with col1:
            st.header("Introduction")
            st.markdown("""
            This analysis tool provides various tools to analyse and interpret data across assessments.
            It is user-friendly and tailored for IMPACT needs.
            """)

            st.subheader("Characteristics")
            st.markdown("""
            - **Data input:** Kobo export + Kobo XLSForm tool  
            - **Analysis options:** Tabular analysis, visualizations, variances  
            - **Customization:** Weighting, strata, etc.
            """)

            st.subheader("Parameters")
            st.markdown("""
            #### Singular Table Output
            - **Variable**: Variable to analyse  
            - **Disaggregated Variable**: Against the first variable  
            - **Weighting**: Yes/No  
            - **Include NA**: Include missing values  
            - **Overall/Strata**: Choose population base  
            - **Minimum Sample**: Threshold for significance  
            """)

        with col2:
            st.subheader("Graph/Table Output Notes")
            st.markdown("""
            - Choose sheet, variable, disaggregation  
            - If variable is select_one/multiple: shows %  
            - If numeric: shows mean, median, min, max  
            """)

            st.subheader("Credits")
            st.markdown("Built by Abraham Azar – abraham.azar30@outlook.com")

    elif nav == "Project Selection":
        st.title("📦 Project Selection")

        if not is_authed():
            st.info("Authenticate in the sidebar to load your deployed projects.")
            st.stop()

        headers_owner = {"Authorization": f"Token {st.session_state.owner_token}"}

        top = st.container(border=True)
        with top:
            st.subheader("Your deployed assets")
            refresh = st.button("🔄 Refresh assets list", type="secondary")

        # Fetch assets once (cached)
        if st.session_state.df_assets is None or refresh:
            with st.status("Fetching assets…", expanded=False) as status:
                count_resp = requests.get(
                    f"{CONFIG['API_ROOT']}/assets/?format=json&limit=1",
                    headers=headers_owner
                )
                if count_resp.status_code != 200:
                    status.update(label="Failed to fetch assets ❌", state="error")
                    st.stop()

                assets_count = count_resp.json().get("count", 0)
                PAGE_SIZE = 100
                frames = []
                fetched = 0

                prog = st.progress(0, text=f"Fetching assets 0/{assets_count}…")
                for offset in range(0, assets_count, PAGE_SIZE):
                    asset_resp = requests.get(
                        f"{CONFIG['API_ROOT']}/assets/?format=json&limit={PAGE_SIZE}&offset={offset}",
                        headers=headers_owner
                    )
                    if asset_resp.status_code != 200:
                        prog.empty()
                        status.update(label="Failed during pagination ❌", state="error")
                        st.stop()

                    assets_page = asset_resp.json().get("results", [])
                    df_page = pd.DataFrame([{
                        "uid": a.get("uid"),
                        "name": a.get("name"),
                        "owner_username": a.get("owner__username"),
                        "deployment_status": a.get("deployment_status"),
                        "kobo_tool": next((d["url"] for d in a.get("downloads", []) if d.get("format") == "xls"), None),
                    } for a in assets_page])

                    frames.append(df_page)
                    fetched += len(assets_page)
                    prog.progress(min(fetched / max(assets_count, 1), 1.0),
                                  text=f"Fetching assets {min(fetched, assets_count)}/{assets_count}…")

                prog.empty()
                st.session_state.df_assets = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()
                status.update(label=f"Assets loaded ✅ ({assets_count})", state="complete")

        df_assets = st.session_state.df_assets.copy()

        owner_assets = df_assets[
            (df_assets["name"].notna()) &
            (df_assets["name"] != "") &
            (df_assets["owner_username"] == st.session_state.owner_username) &
            (df_assets["deployment_status"] == "deployed")
        ].sort_values("name")

        if owner_assets.empty:
            st.warning("No deployed assets found for this user.")
            st.stop()

        sel_box = st.container(border=True)
        with sel_box:
            st.subheader("Choose an asset")
            asset_name = st.selectbox("Asset", owner_assets["name"].tolist(), key="asset_pick")

            selected_uid = owner_assets.loc[owner_assets["name"] == asset_name, "uid"].iloc[0]
            tool_url = owner_assets.loc[owner_assets["name"] == asset_name, "kobo_tool"].iloc[0]

            load_clicked = st.button("✅ Load project", type="primary", use_container_width=True)

        if load_clicked:
            # --- Export + tool/data fetch (commit pattern) ---
            with st.status("Checking exports…", expanded=False) as status:
                check_exports = requests.get(
                    f"{CONFIG['API_ROOT']}/assets/{selected_uid}/exports/?format=json",
                    headers=headers_owner
                )
                if check_exports.status_code != 200:
                    status.update(label="Failed to check exports ❌", state="error")
                    st.stop()

                count_exports = check_exports.json().get("count", 0)

                # create export if none
                if count_exports == 0:
                    status.update(label="No exports found", state="complete")
                    st.info("No exports exist yet for this asset.")
                    if st.button("Create new export", type="primary"):
                        export_payload = {
                            "fields_from_all_versions": "true",
                            "group_sep": "/",
                            "hierarchy_in_labels": "true",
                            "lang": "_xml",
                            "multiple_select": "both",
                            "type": "xls"
                        }
                        create_export = requests.post(
                            f"{CONFIG['API_ROOT']}/assets/{selected_uid}/exports/?format=json",
                            json=export_payload,
                            headers=headers_owner
                        )
                        if create_export.status_code != 201:
                            st.error("Export creation failed.")
                            st.stop()
                        st.success("Export created. Re-loading…")
                        st.rerun()

                exports = check_exports.json().get("results", [])[0]
                latest_export = exports.get("date_created")
                latest_submission = exports.get("last_submission_time")
                data_export_url = exports.get("result")

                status.update(label="Export found ✅", state="complete")

            m1, m2, m3 = st.columns(3)
            with m1:
                st.metric("Asset", asset_name)
            with m2:
                st.metric("Export created", format_time(latest_export))
            with m3:
                st.metric("Last submission", format_time(latest_submission))

            if latest_submission and latest_export and latest_submission > latest_export:
                st.warning("There are submissions not included in the latest export. Create a new export to include them.")
            else:
                st.success("Latest export includes all submissions.")

            tool_url_final = tool_url.replace("/?format=json", "") if isinstance(tool_url, str) else None
            if not tool_url_final:
                st.error("Tool download URL not found for this asset.")
                st.stop()

            with st.status("Downloading tool and data…", expanded=False) as status:
                data_tool = requests.get(tool_url_final, headers=headers_owner)
                data_file = requests.get(data_export_url, headers=headers_owner)

                if data_tool.status_code != 200 or data_file.status_code != 200:
                    status.update(label="Download failed ❌", state="error")
                    st.stop()

                try:
                    df_survey = pd.read_excel(BytesIO(data_tool.content), sheet_name="survey", dtype=str)
                    df_choices = pd.read_excel(BytesIO(data_tool.content), sheet_name="choices", dtype=str)

                    # COMMIT to session state
                    st.session_state.selected_uid = selected_uid
                    st.session_state.selected_asset_name = asset_name
                    st.session_state.survey_df = df_survey
                    st.session_state.choices_df = df_choices
                    st.session_state.data_export_url = data_export_url
                    st.session_state.data_file_bytes = data_file.content
                    st.session_state.label_colname = None

                    # reset analysis state (critical when switching project)
                    reset_analysis_state()

                    status.update(label="Project loaded ✅", state="complete")
                    st.success("Project ready. Go to **Analysis** in the sidebar.")
                    st.rerun()

                except Exception as e:
                    status.update(label="Failed to parse XLS tool ❌", state="error")
                    st.error(f"Could not read survey/choices: {e}")
                    st.stop()

        # Variable summary (shows for loaded project)
        if project_loaded():
            st.markdown("### Variable overview (loaded project)")
            survey_df = st.session_state.survey_df

            def kind_from_type(t: str):
                t = (t or "").strip()
                if t.startswith("select_one"):
                    return "select_one"
                if t.startswith("select_multiple"):
                    return "select_multiple"
                if t in {"integer", "decimal"}:
                    return "numeric"
                return None

            tmp = survey_df[["name", "type"]].copy()
            tmp["kind"] = tmp["type"].apply(kind_from_type)
            tmp = tmp[tmp["kind"].notna()].copy()

            c1, c2, c3 = st.columns(3)
            c1.metric("select_one", int((tmp["kind"] == "select_one").sum()))
            c2.metric("select_multiple", int((tmp["kind"] == "select_multiple").sum()))
            c3.metric("numeric", int((tmp["kind"] == "numeric").sum()))

            with st.expander("See variables list"):
                st.dataframe(tmp[["name", "type", "kind"]], use_container_width=True)

    elif nav == "Analysis":
        st.title("📊 Analysis")
        st.caption(f"Project: {st.session_state.selected_asset_name}")

        survey_df = st.session_state.survey_df
        choices_df = st.session_state.choices_df
        data_bytes = st.session_state.data_file_bytes

        # --- Analysis settings ---
        settings = st.container(border=True)
        with settings:
            st.subheader("Settings")

            label_cols = [c for c in survey_df.columns if isinstance(c, str) and c.startswith("label::")]
            languages = [c.replace("label::", "") for c in label_cols]

            if not label_cols:
                st.warning("This tool has no label:: columns. Labels will not be available.")
                st.session_state.label_colname = None
            else:
                default_lang = "English" if "English" in languages else languages[0]
                default_index = languages.index(default_lang)
                selected_language = st.selectbox("Label language", languages, index=default_index, key="label_lang_widget")
                st.session_state.label_colname = f"label::{selected_language}"

        label_col = st.session_state.label_colname

        tab1, tab2 = st.tabs(["Table", "Graph"])

        with tab1:
            st.subheader("📊 Table Output / Variance")

            col1, col2, col3 = st.columns(3)

            with col1:
                try:
                    sheet_names = pd.ExcelFile(BytesIO(data_bytes)).sheet_names
                    selected_sheet = st.selectbox("Select Sheet", sheet_names, key="analysis_sheet_widget")
                    st.session_state.analysis_sheet = selected_sheet
                except Exception as e:
                    st.error(f"Error reading data file: {e}")
                    st.stop()

            with col2:
                variable = None
                if selected_sheet:
                    try:
                        df_data = pd.read_excel(BytesIO(data_bytes), sheet_name=selected_sheet, dtype=str)

                        metadata_cols = {"start", "end", "today", "deviceid", "audit", "audit_url", "auditurl"}
                        df_data = df_data[[c for c in df_data.columns if str(c).lower() not in metadata_cols]]

                        non_empty_cols = [c for c in df_data.columns if not df_data[c].isna().all()]
                        supported_cols = []
                        for c in non_empty_cols:
                            kind, _ = get_var_kind_and_listname(survey_df, c)
                            if kind in {"select_one", "select_multiple", "numeric"}:
                                supported_cols.append(c)

                        supported_cols = sorted(supported_cols)

                        variable = st.selectbox("Select Variable", supported_cols, key="analysis_variable_widget")
                        st.session_state.analysis_variable = variable
                    except Exception as e:
                        st.error(f"Error loading sheet data: {e}")
                        st.stop()

            with col3:
                disaggregation = "No disaggregation"
                if selected_sheet and variable:
                    try:
                        # reuse supported_cols from above by re-reading quickly (safe/simple)
                        df_data = pd.read_excel(BytesIO(data_bytes), sheet_name=selected_sheet, dtype=str)
                        metadata_cols = {"start", "end", "today", "deviceid", "audit", "audit_url", "auditurl"}
                        df_data = df_data[[c for c in df_data.columns if str(c).lower() not in metadata_cols]]
                        non_empty_cols = [c for c in df_data.columns if not df_data[c].isna().all()]

                        supported_cols = []
                        for c in non_empty_cols:
                            kind, _ = get_var_kind_and_listname(survey_df, c)
                            if kind in {"select_one", "select_multiple", "numeric"}:
                                supported_cols.append(c)
                        supported_cols = sorted(supported_cols)

                        disagg_options = ["No disaggregation"] + [c for c in supported_cols if c != variable]
                        disaggregation = st.selectbox("Disaggregation Variable", disagg_options, key="analysis_disagg_widget")
                        st.session_state.analysis_disagg = disaggregation
                    except Exception as e:
                        st.error(f"Error loading disaggregation options: {e}")
                        st.stop()

            # st.markdown("### Parameters")
            # p1, p2, p3 = st.columns(3)
            # with p1:
            #     weight = st.radio("Weighting", ["no", "yes"], key="weight_widget")
            # with p2:
            #     calc_na = st.radio("Calculation", ["None", "Include NA"], key="na_widget")
            # with p3:
            #     level = st.radio("Level", ["overall", "strata"], key="level_widget")

            # st.markdown("### Output Preview")

            if selected_sheet and variable:
                try:
                    df_data = pd.read_excel(BytesIO(data_bytes), sheet_name=selected_sheet, dtype=str)
                    metadata_cols = {"start", "end", "today", "deviceid", "audit", "audit_url", "auditurl"}
                    df_data = df_data[[c for c in df_data.columns if str(c).lower() not in metadata_cols]]

                    var_type, var_list_name = get_var_kind_and_listname(survey_df, variable)

                    # Relabel disaggregation values if select_one
                    no_disagg = (disaggregation == "No disaggregation")
                    if not no_disagg and disaggregation in df_data.columns:
                        disagg_type, disagg_list_name = get_var_kind_and_listname(survey_df, disaggregation)
                        if disagg_type == "select_one" and disagg_list_name and label_col:
                            label_map = build_label_map(choices_df, disagg_list_name, label_col)
                            df_data[disaggregation] = df_data[disaggregation].astype(str).map(label_map).fillna(df_data[disaggregation])

                    # Relabel main variable values if select_one
                    if var_type == "select_one" and label_col:
                        df_data[variable] = relabel_select_one_series(df_data, survey_df, choices_df, variable, label_col)

                    if var_type == "select_one":
                        if no_disagg:
                            s = df_data[variable].fillna("NA")
                            counts = s.value_counts(dropna=False)
                            total = counts.sum()
                            result = pd.DataFrame({
                                "response": counts.index.astype(str),
                                "pct": (counts / total * 100).round(2).astype(str) + "%",
                                "num_samples": counts.values
                            })
                        else:
                            ctab = pd.crosstab(df_data[disaggregation], df_data[variable], margins=False, dropna=False)
                            ctab.index.name = disaggregation
                            ctab.reset_index(inplace=True)
                            ctab["num_samples"] = ctab.drop(columns=[disaggregation]).sum(axis=1)
                            for col in ctab.columns[1:-1]:
                                ctab[col] = (ctab[col] / ctab["num_samples"] * 100).round(2).astype(str) + "%"
                            result = ctab

                    elif var_type == "select_multiple":
                        sm_cols_raw = [c for c in df_data.columns if str(c).startswith(variable + "/")]
                        if not sm_cols_raw:
                            st.warning("No select_multiple option columns found for this variable in the export.")
                            st.stop()

                        df_sm = df_data[([disaggregation] if not no_disagg else []) + sm_cols_raw].copy()
                        df_sm = df_sm.rename(columns=lambda c: str(c).replace("/", "___"))
                        sm_cols = [str(c).replace("/", "___") for c in sm_cols_raw]

                        df_sm[sm_cols] = df_sm[sm_cols].apply(pd.to_numeric, errors="coerce")

                        # Optional: relabel option names using choices
                        label_map = build_label_map(choices_df, var_list_name, label_col) if (label_col and var_list_name) else {}
                        def pretty_sm(colname: str) -> str:
                            # colname is "variable___option"
                            opt = colname.split("___", 1)[1] if "___" in colname else colname
                            return label_map.get(opt, opt)

                        if no_disagg:
                            means = df_sm[sm_cols].mean(skipna=True)
                            counts = df_sm[sm_cols].count()
                            result = pd.DataFrame({
                                "option": [pretty_sm(o) for o in means.index],
                                "pct": (means * 100).round(2).astype(str) + "%",
                                "num_samples": counts.values
                            })
                        else:
                            grouped = df_sm.groupby(disaggregation)[sm_cols].agg(["mean", "count"])
                            grouped.columns = [
                                f"{pretty_sm(col)}_pct" if stat == "mean" else "num_samples"
                                for col, stat in grouped.columns
                            ]
                            grouped = grouped.reset_index()
                            for col in grouped.columns:
                                if str(col).endswith("_pct"):
                                    grouped[col] = (grouped[col].astype(float) * 100).round(2).astype(str) + "%"
                            result = grouped

                    elif var_type == "numeric":
                        df_data[variable] = pd.to_numeric(df_data[variable], errors="coerce")
                        if no_disagg:
                            s = df_data[variable]
                            result = pd.DataFrame([{
                                "num_samples": int(s.count()),
                                "mean": round(s.mean(), 2) if s.count() else None,
                                "median": round(s.median(), 2) if s.count() else None,
                                "min": round(s.min(), 2) if s.count() else None,
                                "max": round(s.max(), 2) if s.count() else None,
                            }])
                        else:
                            result = (
                                df_data.groupby(disaggregation)[variable]
                                .agg(["count", "mean", "median", "min", "max"])
                                .reset_index()
                                .rename(columns={"count": "num_samples"})
                            )
                            result[["mean", "median", "min", "max"]] = result[["mean", "median", "min", "max"]].round(2)

                    else:
                        st.warning("Unsupported variable type or missing type information.")
                        st.stop()

                    st.dataframe(result, use_container_width=True)

                except Exception as e:
                    st.error(f"Failed to generate preview: {e}")
            else:
                st.info("Select a sheet and a variable to see a table preview.")

        with tab2:
            st.subheader("📈 Graph Output")

            df_survey = st.session_state.survey_df
            df_choices = st.session_state.choices_df
            data_bytes = st.session_state.data_file_bytes  # IMPORTANT: bytes

            # Reuse existing selections from the Table tab if already chosen
            sheet_names = pd.ExcelFile(BytesIO(data_bytes)).sheet_names

            g1, g2, g3 = st.columns(3)
            with g1:
                sheet_graph = st.selectbox(
                    "Sheet",
                    sheet_names,
                    index=sheet_names.index(st.session_state.analysis_sheet) if st.session_state.analysis_sheet in sheet_names else 0,
                    key="graph_sheet_widget"
                )

            # Load data once for graph UI
            df_data = pd.read_excel(BytesIO(data_bytes), sheet_name=sheet_graph, dtype=str)
            metadata_cols = {"start", "end", "today", "deviceid", "audit", "audit_url", "auditurl"}
            df_data = df_data[[c for c in df_data.columns if str(c).lower() not in metadata_cols]]

            non_empty_cols = [c for c in df_data.columns if not df_data[c].isna().all()]
            supported_cols = []
            for c in non_empty_cols:
                kind, _ = get_var_kind_and_listname(df_survey, c)
                if kind in {"select_one", "select_multiple", "numeric"}:
                    supported_cols.append(c)
            supported_cols = sorted(supported_cols)

            with g2:
                if not supported_cols:
                    st.error("No supported variables found on this sheet (tool vs export mismatch or empty columns).")
                    st.stop()

                default_var = st.session_state.analysis_variable if st.session_state.analysis_variable in supported_cols else supported_cols[0]
                var_graph = st.selectbox("Variable", supported_cols, index=supported_cols.index(default_var), key="graph_var_widget")

            with g3:
                disagg_options = ["No disaggregation"] + [c for c in supported_cols if c != var_graph]
                default_dis = st.session_state.analysis_disagg if st.session_state.analysis_disagg in disagg_options else "No disaggregation"
                dis_graph = st.selectbox("Disaggregation", disagg_options, index=disagg_options.index(default_dis), key="graph_dis_widget")

            st.markdown("### Parameters")
            p1, p2, p3 = st.columns(3)
            with p1:
                weight_graph = st.radio("Weighting", ["no", "yes"], key="weight_graph_widget")
            with p2:
                calc_graph = st.radio("Calculation", ["None", "Include NA"], key="calc_graph_widget")
            with p3:
                admin_graph = st.radio("Level", ["overall", "strata"], key="admin_graph_widget")

            # Guardrails like your R textPlot
            if admin_graph == "strata" and "strata" not in df_data.columns:
                st.warning("Missing 'strata' column in this sheet. Strata-level graphs are disabled for this sheet.")
                admin_graph = "overall"

            if weight_graph == "yes" and "weight" not in df_data.columns:
                st.warning("Missing 'weight' column in this sheet. Weighting will be ignored.")
                weight_graph = "no"

            omit_na = (calc_graph != "Include NA")
            label_col = st.session_state.label_colname

            # Determine if disagg is numeric (for scatter gating)
            disagg_is_numeric = False
            if dis_graph != "No disaggregation":
                dis_kind, _ = get_var_kind_and_listname(df_survey, dis_graph)
                disagg_is_numeric = (dis_kind == "numeric")

            # Compute tidy graph data
            tidy, var_type = compute_graph_tidy(
                df_data=df_data,
                survey_df=df_survey,
                choices_df=df_choices,
                variable=var_graph,
                disaggregation=dis_graph,
                label_col=label_col,
                omit_na=omit_na
            )

            if tidy.empty or var_type is None:
                st.info("No data available for this graph selection.")
                st.stop()

            # Offer graph type choices (mirrors your R logic)
            has_disagg = (dis_graph != "No disaggregation")
            choices = graph_type_choices(var_type, has_disagg, admin_graph, disagg_is_numeric)
            chart_kind = st.radio("Choose Graph", choices, horizontal=True, key="graph_kind_widget")

            # -------------------------
            # Render charts (Plotly)
            # -------------------------
            fig = None

            if not has_disagg:
                if var_type == "numeric":
                    if chart_kind == "bar":
                        # show mean only like your R correlation_table filter(names=="mean")
                        mean_val = float(tidy.loc[tidy["metric"] == "mean", "value"].iloc[0])
                        fig = px.bar(pd.DataFrame({"metric": ["mean"], "value": [mean_val]}), x="metric", y="value")
                    elif chart_kind == "gauge":
                        v_mean = float(tidy.loc[tidy["metric"] == "mean", "value"].iloc[0])
                        v_min  = float(tidy.loc[tidy["metric"] == "min", "value"].iloc[0])
                        v_max  = float(tidy.loc[tidy["metric"] == "max", "value"].iloc[0])
                        fig = go.Figure(go.Indicator(
                            mode="gauge+number",
                            value=v_mean,
                            gauge={"axis": {"range": [v_min, v_max]}}
                        ))
                else:
                    # select_one / select_multiple
                    if chart_kind == "bar":
                        tidy_sorted = tidy.sort_values("value")
                        fig = px.bar(tidy_sorted, x="value", y="category", orientation="h")
                    elif chart_kind == "line":
                        fig = px.line(tidy, x="category", y="value", markers=True)
                    elif chart_kind == "pie":
                        fig = px.pie(tidy, names="category", values="value")
                    elif chart_kind == "donut":
                        fig = px.pie(tidy, names="category", values="value", hole=0.5)

            else:
                # With disaggregation
                if var_type == "numeric":
                    if chart_kind == "bar":
                        # show mean only by disagg
                        mean_only = tidy[tidy["metric"] == "mean"].copy()
                        mean_only = mean_only.sort_values("value")
                        fig = px.bar(mean_only, x="value", y="disagg", orientation="h")
                    elif chart_kind == "scatter" and disagg_is_numeric:
                        # true scatter of raw points
                        # Need raw numeric columns
                        df_sc = df_data[[var_graph, dis_graph]].copy()
                        df_sc[var_graph] = pd.to_numeric(df_sc[var_graph], errors="coerce")
                        df_sc[dis_graph] = pd.to_numeric(df_sc[dis_graph], errors="coerce")
                        df_sc = df_sc.dropna()
                        fig = px.scatter(df_sc, x=var_graph, y=dis_graph)
                else:
                    # select_* with disagg
                    if chart_kind == "heatmap":
                        # tidy: disagg x category -> value
                        pivot = tidy.pivot_table(index="category", columns="disagg", values="value", aggfunc="mean").fillna(0)
                        fig = px.imshow(pivot.values, x=pivot.columns.astype(str), y=pivot.index.astype(str))
                    elif chart_kind == "bar":
                        # stacked bar: disagg on y, category stacked
                        # easiest: pivot and then plot
                        pivot = tidy.pivot_table(index="disagg", columns="category", values="value", aggfunc="mean").fillna(0)
                        plot_df = pivot.reset_index()
                        plot_long = plot_df.melt(id_vars=["disagg"], var_name="category", value_name="value")
                        fig = px.bar(plot_long, x="value", y="disagg", color="category", orientation="h")

            if fig is None:
                st.info("This graph type isn’t implemented for the current selection yet.")
            else:
                fig.update_layout(
                    margin=dict(l=10, r=10, t=30, b=10),
                    yaxis_title=None
                )
                st.plotly_chart(fig, use_container_width=True)

