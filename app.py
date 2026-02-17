import streamlit as st
import pandas as pd
import requests
from PIL import Image
from io import BytesIO
from datetime import datetime

# --------------
# APP SETTINGS
# --------------
st.set_page_config(page_title="Analysis App", layout="wide")

# --------------
# MANUAL CREDENTIALS
# --------------
USER_CREDENTIALS = {
    "admin": {
        "password": "admin"
    },
    "drc_user": {
        "password": "kobo@drc@2O26",
    }
}

# --------------
# SESSION INIT
# --------------
if "logged_in" not in st.session_state:
    st.session_state.logged_in = False
    st.session_state.username = ""

# --------------
# FUNCTIONS
# --------------
def logout():
    st.session_state.logged_in = False
    st.session_state.username = ""


def format_time(time_string):
    time_string = datetime.fromisoformat(time_string.replace("Z", "+00:00"))
    return time_string.strftime("%Y-%m-%d %H:%M:%S")

def get_var_kind_and_listname(survey_df: pd.DataFrame, varname: str):
    """Return (kind, list_name). kind in {'select_one','select_multiple','numeric', None}"""
    if not varname or varname == "No disaggregation":
        return None, None

    row = survey_df.loc[survey_df["name"] == varname]
    if row.empty:
        return None, None

    t = str(row.iloc[0].get("type", "")).strip()

    if t.startswith("select_one"):
        parts = t.split(maxsplit=1)
        return "select_one", (parts[1] if len(parts) == 2 else None)

    if t.startswith("select_multiple"):
        parts = t.split(maxsplit=1)
        return "select_multiple", (parts[1] if len(parts) == 2 else None)

    if t in {"integer", "decimal"}:
        return "numeric", None

    return None, None


def build_label_map(choices_df: pd.DataFrame, list_name: str, label_col: str):
    """Map choice code -> label for a given list_name."""
    if not list_name:
        return {}

    df = choices_df[
        (choices_df["list_name"] == list_name) & (choices_df["name"].notna())
    ].copy()

    if label_col not in df.columns:
        return {}

    return dict(zip(df["name"].astype(str), df[label_col].astype(str)))

# --------------
# LOGIN FORM
# --------------
if not st.session_state.logged_in:
    logo = Image.open("www/logo_beginning.png")
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
                st.success(f"Welcome!")
                st.rerun()
            else:
                st.error("Invalid username or password.")

# --------------
# MAIN APP AFTER LOGIN
# --------------
# Placeholder for session state

for key in ["label_colname", "survey_df", "data_export", "data_file",
            "choice_df", "table_preview", "kobo_url", "owner_token",
            "owner_username", "owner_assets"]:
    if key not in st.session_state:
        st.session_state[key] = None

if st.session_state.logged_in:
    st.sidebar.success(f"Welcome!")


    kobo_url = st.sidebar.text_input("Please enter the kobo kpi", value = "https://kobo.drc.ngo")
    st.session_state.kobo_url = kobo_url

    CONFIG = {
        "API_ROOT": f"{st.session_state.kobo_url}/api/v2"
    }


    # Navigation Tabs
    tab = st.sidebar.radio("Navigation", [
        "Read Me", "Authentication and Project Selection", 
        "Singular Table Output / Variance", 
        "Singular Graph Output"
    ])

    if st.sidebar.button("Logout"):
        logout()
        st.rerun()

    # Content for each section
    if tab == "Read Me":
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
            - **Data input:** Upload cleaned data and Kobo Tools (.xlsx)  
            - **Analysis options:** Tabular analysis, visualizations, variances, key highlights  
            - **Customization:** Add weighting, strata, and more
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

            st.markdown("""
            #### Singular Graph Output
            - Same parameters as above  
            """)

        with col2:
            st.subheader("Methodology")
            st.markdown("""
            - Upload cleaned data and Kobo tool  
            """)

            st.subheader("Graph/Table Output Notes")
            st.markdown("""
            - Choose sheet, variable, disaggregation  
            - If variable is select_one/multiple: shows %  
            - If numeric: shows mean, median, min, max  
            """)

            st.subheader("Requirements")
            st.markdown("""
            - Excel format with main sheets + survey & choices  
            - Weight and strata columns for respective features  
            """)

            st.subheader("Credits")
            st.markdown("""
            Built by Abraham Azar – abraham.azar30@outlook.com 
            """)

    elif tab == "Authentication and Project Selection":
        if ("owner_username" not in st.session_state or st.session_state.owner_username is None):
            auth_box = st.empty()
            with auth_box.form(key="token_form", clear_on_submit=True):
                st.subheader("🔐 API Token")
                owner_token = st.text_input("Owner User Token", placeholder="Enter owner's API Token", type="password")
                submit_token = st.form_submit_button("Authenticate")

                if submit_token:
                    headers_owner = {"Authorization": f"Token {owner_token}"}
                    owner_resp = requests.get(f"{CONFIG['API_ROOT']}/access-logs/me/?format=json&limit=1", headers=headers_owner)

                    if owner_resp.status_code == 200:
                        st.session_state.owner_token = owner_token
                        st.session_state.owner_username = owner_resp.json()['results'][0]['username']
                        auth_box.empty()
                        st.rerun()
                    else:
                        st.error("❌ Invalid token. Please try again")

        
        if st.session_state.owner_username:
            st.subheader("✅ Authenticated User")
            st.markdown("**👤 owner Username**")
            st.info(st.session_state.owner_username)
            
                # ------ FETCH owner'S ASSETS (once, with progress) ------
            headers_owner = {"Authorization": f"Token {st.session_state.owner_token}"}

             # Button to explicitly refresh data if needed
            refresh = st.button("🔄 Refresh assets list")

            if "df_assets" not in st.session_state or refresh:
                # First, get count cheaply (limit=1 keeps payload tiny)
                count_resp = requests.get(f"{CONFIG['API_ROOT']}/assets/?format=json&limit=1", headers=headers_owner)
                if count_resp.status_code != 200:
                    st.error("❌ Failed to fetch owner's assets count.")
                    st.stop()

                assets_count = count_resp.json().get("count", 0)
                if assets_count == 0:
                    st.session_state.df_assets = pd.DataFrame(
                        columns=["uid", "name", "owner_username", "deployment_status"]
                    )
                else:
                    PAGE_SIZE = 100
                    frames = []

                    prog = st.progress(0, text=f"Fetching assets 0/{assets_count}…")
                    fetched = 0

                    for offset in range(0, assets_count, PAGE_SIZE):
                        asset_resp = requests.get(
                            f"{CONFIG['API_ROOT']}/assets/?format=json&limit={PAGE_SIZE}&offset={offset}",
                            headers=headers_owner
                        )
                        if asset_resp.status_code != 200:
                            prog.empty()
                            st.error(f"❌ Failed at offset {offset}: {asset_resp.status_code} - {asset_resp.reason}")
                            st.stop()

                        assets_page = asset_resp.json().get("results", [])
                        df_page = pd.DataFrame([
                            {
                                "uid": a.get("uid"),
                                "name": a.get("name"),
                                "owner_username": a.get("owner__username"),
                                "deployment_status": a.get("deployment_status"),
                                "kobo_tool": next(
                                                (d["url"] for d in a.get("downloads", []) if d.get("format") == "xls"),
                                                None
                                            ),
                            }
                            for a in assets_page
                        ])
                        frames.append(df_page)

                        # update progress by items fetched (safer than inferring from offsets)
                        fetched += len(assets_page)
                        prog.progress(min(fetched / max(assets_count, 1), 1.0),
                                    text=f"Fetching assets {min(fetched, assets_count)}/{assets_count}…")

                    prog.empty()
                    st.session_state.df_assets = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame(
                        columns=["uid", "name", "owner_username", "deployment_status"]
                    )

            # From here on, just reuse the cached DataFrame — no re-fetch on widget changes
            df_assets = st.session_state.df_assets

            owner_assets = df_assets[
                (df_assets["name"] != "") &
                (df_assets["owner_username"] == st.session_state.owner_username) &
                (df_assets["deployment_status"] == "deployed")
            ]

            if not owner_assets.empty:
                selected_names = st.selectbox(
                    "📦 Select assets to analyze:",
                    options=owner_assets["name"].tolist()
                )

                selected_uid = owner_assets[owner_assets["name"] == selected_names]["uid"].iloc[0]
                if selected_uid is not None:
                    check_exports = requests.get(f"{CONFIG['API_ROOT']}/assets/{selected_uid}/exports/?format=json", headers=headers_owner)
                    if check_exports.status_code == 200:
                        count_exports = check_exports.json().get("count")
                        if count_exports == 0:
                            st.info("No exports are created yet.")
                            if st.button("Create new export"):
                                export_payload = {
                                    "fields_from_all_versions": "true",
                                    "group_sep": "/",
                                    "hierarchy_in_labels": "true",
                                    "lang": "_xml",
                                    "multiple_select": "both",
                                    "type": "xls"
                                }
                                create_export = requests.post(f"{CONFIG["API_ROOT"]}/assets/{selected_uid}/exports/?format=json", json=export_payload, headers=headers_owner)
                                if create_export.status_code == 201:
                                    check_exports_after_created = requests.get(f"{CONFIG['API_ROOT']}/assets/{selected_uid[0]}/exports/?format=json", headers=headers_owner)


                                    if check_exports_after_created.status_code == 200:
                                        exports = check_exports_after_created.json().get("results", [])[0]
                                        latest_export = exports.get("date_created")
                                        latest_export_readable = format_time(latest_export)
                                        latest_submission = exports.get("last_submission_time")
                                        latest_submission_readable = format_time(latest_submission)
                                        data_export = exports.get("result")
                                        st.session_state.data_export = data_export
                                        type_export = exports.get("data", None).get("lang")
                                        if latest_submission > latest_export:
                                            st.badge("There are submissions not included in your latest export. If you want create a new export to catch all submissions.")
                                        else:
                                            st.badge("Your latest export include all submissions.")
                                st.rerun()
                        else:
                            check_exports = requests.get(f"{CONFIG['API_ROOT']}/assets/{selected_uid}/exports/?format=json", headers=headers_owner)
                            if check_exports.status_code == 200:
                                exports = check_exports.json().get("results", [])[0]
                                latest_export = exports.get("date_created")
                                latest_export_readable = format_time(latest_export)
                                latest_submission = exports.get("last_submission_time")
                                latest_submission_readable = format_time(latest_submission)
                                data_export = exports.get("result")
                                st.session_state.data_export = data_export
                                type_export = exports.get("data", None).get("lang")
                                if latest_submission > latest_export:
                                    st.badge("There are submissions not included in your latest export. If you want create a new export to catch all submissions.")
                                else:
                                    st.badge("Your latest export include all submissions.")

                    else:
                        st.error("Issue with checking the exports of the selected asset.")




                tool_url = df_assets[df_assets["uid"] == selected_uid]["kobo_tool"].iloc[0]
                tool_url_final = tool_url.replace("/?format=json","")

                data_tool = requests.get(tool_url_final, headers=headers_owner)

                data_file = requests.get(data_export, headers=headers_owner)


                
                if data_tool.status_code == 200 and data_file.status_code == 200:
                    try:
                        # Read the "survey" sheet as text
                        df_survey = pd.read_excel(BytesIO(data_tool.content), sheet_name="survey", dtype=str)
                        df_choices = pd.read_excel(BytesIO(data_tool.content), sheet_name="choices", dtype=str)
                        st.session_state.survey_df = df_survey
                        st.session_state.choices_df = df_choices
                        st.session_state.data_file = data_file
                        # Extract columns that start with "label::"
                        label_cols = [col for col in df_survey.columns if col.startswith("label::")]
                        st.success("Successfully extracted Tool Survey")
                        st.success("Successfully extracted Data File")
                        if label_cols:
                            # Strip "label::" to get language codes
                            languages = [col.replace("label::", "") for col in label_cols]
                            selected_language = st.selectbox("Select Language", languages)

                            # Store the full column name (e.g., "label::English")
                            label_colname = f"label::{selected_language}"
                            st.session_state["label_colname"] = label_colname

                        else:
                            st.warning("No 'label::' columns found in survey sheet.")

                    except Exception as e:
                        st.error(f"Could not read survey sheet: {e}")
                else:
                    st.info("Please upload the Kobo Tool to select language.")

    elif tab == "Singular Table Output / Variance":


        st.title("📊 Table Output / Variance")

        if (st.session_state.survey_df is not None or st.session_state.choices_df is not None) and st.session_state.data_file is not None:
            df_survey = st.session_state.survey_df 
            df_choices = st.session_state.choices_df
            data_file = st.session_state.data_file
            col1, col2, col3 = st.columns(3)

            with col1:
                if st.session_state.data_file:
                    try:
                        sheet_names = pd.ExcelFile(BytesIO(data_file.content)).sheet_names
                        selected_sheet = st.selectbox("Select Sheet", sheet_names)
                    except Exception as e:
                        st.error(f"Error reading data file: {e}")

            with col2:
                if st.session_state.data_file and 'selected_sheet' in locals():
                    try:
                        df_data = pd.read_excel(BytesIO(data_file.content), sheet_name=selected_sheet, dtype=str)
                        metadata_cols = ["start", "end", "today", "deviceid", "audit", "audit_URL", "auditURL"]
                        df_data = df_data[[col for col in df_data.columns if col.lower() not in metadata_cols]]

                        non_empty_cols = [col for col in df_data.columns if not df_data[col].isna().all()]
                        supported_cols = []
                        for c in non_empty_cols:
                            kind, _ = get_var_kind_and_listname(df_survey, c)
                            if kind in {"select_one", "select_multiple", "numeric"}:
                                supported_cols.append(c)

                        variable = st.selectbox("Select Variable", supported_cols)
                    except Exception as e:
                        st.error(f"Error loading sheet data: {e}")
                
            with col3:
                if st.session_state.data_file and 'selected_sheet' in locals():
                    try:
                        df_data = pd.read_excel(BytesIO(data_file.content), sheet_name=selected_sheet, dtype=str)
                        metadata_cols = ["start", "end", "today", "deviceid", "audit", "audit_URL"]
                        df_data = df_data[[col for col in df_data.columns if col not in metadata_cols]]

                        non_empty_cols = [col for col in df_data.columns if not df_data[col].isna().all()]

                        
                        supported_cols = []
                        for c in non_empty_cols:
                            kind, _ = get_var_kind_and_listname(df_survey, c)
                            if kind in {"select_one", "select_multiple", "numeric"}:
                                supported_cols.append(c)

                        disagg_options = ["No disaggregation"] + [c for c in supported_cols if c != variable]
                        disaggregation = st.selectbox("Disaggregation Variable", disagg_options)
                    except Exception as e:
                        st.error(f"Error loading sheet data: {e}")

            # Parameter Section
            st.markdown("### Parameters")
            col1, col2, col3 = st.columns(3)
            with col1:
                weight = st.radio("Weighting", ["no", "yes"])
            with col2:
                calc_na = st.radio("Calculation", ["None", "Include NA"])
            with col3:
                level = st.radio("Level", ["overall", "strata"])

            # Output preview
            st.markdown("### Output Preview")
            
            if st.session_state.data_file and 'variable' in locals():
                try:
                    df_data = pd.read_excel(BytesIO(data_file.content), sheet_name=selected_sheet, dtype=str)
                    survey_df = st.session_state.survey_df
                    choices_df = st.session_state.choices_df
                    label_col = st.session_state.label_colname


                    var_type_row = survey_df[survey_df["name"] == variable]
                    var_type = None
                    if not var_type_row.empty:
                        var_type_full = var_type_row.iloc[0]["type"]
                        if var_type_full.startswith("select_one"):
                            var_type = "select_one"
                        elif var_type_full.startswith("select_multiple"):
                            var_type = "select_multiple"
                        elif var_type_full in ["integer", "decimal"]:
                            var_type = "numeric"

                    # Handle disaggregation label mapping if select_one
                    disagg_row = survey_df[survey_df["name"] == disaggregation]
                    if not disagg_row.empty:
                        disagg_type = disagg_row.iloc[0]["type"]
                        disagg_parts = disagg_type.split()
                        if len(disagg_parts) == 2 and disagg_parts[0] == "select_one":
                            disagg_list_name = disagg_parts[1]
                            if disagg_list_name in choices_df["list_name"].values:
                                label_map_df = choices_df[(choices_df["list_name"] == disagg_list_name) & (choices_df["name"].notna())]
                                if label_col in label_map_df.columns:
                                    label_map = dict(zip(label_map_df["name"], label_map_df[label_col]))
                                    df_data[disaggregation] = df_data[disaggregation].map(label_map).fillna(df_data[disaggregation])

                    # Process table generation based on var_type
                    no_disagg = (disaggregation == "No disaggregation")

                    if var_type == "select_one":
                        if no_disagg:
                            counts = df_data[variable].fillna("NA").value_counts(dropna=False)
                            total = counts.sum()
                            result = pd.DataFrame({
                                variable: counts.index.astype(str),
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
                        sm_cols = [col for col in df_data.columns if col.startswith(variable + "/")]
                        df_sm = df_data[([disaggregation] if not no_disagg else []) + sm_cols].copy()
                        df_sm = df_sm.rename(columns=lambda c: c.replace("/", "___"))
                        sm_cols = [c.replace("/", "___") for c in sm_cols]

                        df_sm[sm_cols] = df_sm[sm_cols].apply(pd.to_numeric, errors="coerce")

                        if no_disagg:
                            means = df_sm[sm_cols].mean(skipna=True)
                            counts = df_sm[sm_cols].count()
                            result = pd.DataFrame({
                                "option": means.index,
                                "pct": (means * 100).round(2).astype(str) + "%",
                                "num_samples": counts.values
                            })
                        else:
                            grouped = df_sm.groupby(disaggregation)[sm_cols].agg(["mean", "count"])
                            grouped.columns = [
                                f"{col}_pct" if stat == "mean" else "num_samples"
                                for col, stat in grouped.columns
                            ]
                            grouped = grouped.reset_index()
                            for col in grouped.columns:
                                if col.endswith("_pct"):
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


                    st.dataframe(result)
                except Exception as e:
                    st.error(f"Failed to generate preview: {e}")
            else:
                st.info("Upload data and tool files, and select a variable to see a table preview.")
            #         # Check variable type and label
            #         var_label = variable
            #         var_type = None
            #         list_name = None
            #         if survey_df is not None:
            #             matched_row = survey_df[survey_df["name"] == variable]
            #             if not matched_row.empty:
            #                 qtype_parts = matched_row.iloc[0]["type"].split()
            #                 var_type = qtype_parts[0]
            #                 list_name = qtype_parts[1] if len(qtype_parts) > 1 else None
            #                 if label_col in matched_row.columns:
            #                     var_label = matched_row.iloc[0][label_col]

            #         # Map value labels for select_one
            #         if var_type == "select_one" and list_name and label_col in choices_df.columns:
            #             label_map = choices_df[choices_df["list_name"] == list_name][["name", label_col]].dropna()
            #             label_map_dict = dict(zip(label_map["name"], label_map[label_col]))
            #             df_data[variable] = df_data[variable].map(label_map_dict).fillna(df_data[variable])

            #         # Generate disaggregated table
            #         if disaggregation != "No disaggregation" and disaggregation in df_data.columns:
            #             disagg_row = survey_df[survey_df["name"] == disaggregation]
            #             if not disagg_row.empty:
            #                 disagg_type = disagg_row.iloc[0]["type"]
            #                 disagg_parts = disagg_type.split()
            #                 if len(disagg_parts) == 2 and disagg_parts[0] == "select_one":
            #                     disagg_list_name = disagg_parts[1]
            #                     if disagg_list_name in choices_df["list_name"].values:
            #                         label_map_df = choices_df[(choices_df["list_name"] == disagg_list_name) & (choices_df["name"].notna())]
            #                         if label_col in label_map_df.columns:
            #                             label_map = dict(zip(label_map_df["name"], label_map_df[label_col]))
            #                             df_data[disaggregation] = df_data[disaggregation].map(label_map).fillna(df_data[disaggregation])

            #             # Generate crosstab table
            #             cross_tab = pd.crosstab(df_data[disaggregation], df_data[variable], margins=False, dropna=False)
            #             cross_tab_percent = cross_tab.div(cross_tab.sum(axis=1), axis=0).multiply(100).round(1)
            #             cross_tab.insert(0, "num_samples", cross_tab.sum(axis=1))
            #             for col in cross_tab_percent.columns:
            #                 cross_tab[col] = cross_tab_percent[col].astype(str) + "%"
            #         else:
            #             counts = df_data[variable].value_counts(dropna=False)
            #             cross_tab = pd.DataFrame({"num_samples": [counts.sum()]})
            #             for val, count in counts.items():
            #                 cross_tab[val] = [f"{(count / counts.sum() * 100):.1f}%"]

            #         # Show the label and response count
            #         st.markdown(f"#### 1. {var_label}")
            #         answered = df_data[variable].notna().sum()
            #         total = len(df_data)
            #         st.markdown(f"**Variable name:** `{variable}`")
            #         st.markdown(f"*{(answered / total * 100):.1f}% of respondents answered this question.*")
            #         cross_tab.columns = [str(col) if pd.notna(col) else "—" for col in cross_tab.columns]
            #         cross_tab.index = [str(idx) if pd.notna(idx) else "—" for idx in cross_tab.index]
            #         st.dataframe(cross_tab.fillna("—"))

            #     except Exception as e:
            #         st.error(f"Failed to generate preview: {e}")
            # else:
            #     st.info("Upload data and tool files, and select a variable to see a table preview.")



    elif tab == "Singular Graph Output":
        st.title("📈 Graph Output")
        st.info("Coming soon: logic for graphical analysis.")
