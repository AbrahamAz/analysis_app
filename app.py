import streamlit as st
import pandas as pd
from PIL import Image

# --------------
# APP SETTINGS
# --------------
st.set_page_config(page_title="Analysis App", layout="wide")

# --------------
# MANUAL CREDENTIALS
# --------------
USER_CREDENTIALS = {
    "admin": {
        "password": "admin",
        "name": "User One",
        "permissions": "admin"
    },
    "user": {
        "password": "123456",
        "name": "User Two",
        "permissions": "standard"
    }
}

# --------------
# SESSION INIT
# --------------
if "logged_in" not in st.session_state:
    st.session_state.logged_in = False
    st.session_state.username = ""
    st.session_state.name = ""
    st.session_state.permissions = ""

# --------------
# LOGOUT FUNCTION
# --------------
def logout():
    st.session_state.logged_in = False
    st.session_state.username = ""
    st.session_state.name = ""
    st.session_state.permissions = ""

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
                st.session_state.name = USER_CREDENTIALS[username]["name"]
                st.session_state.permissions = USER_CREDENTIALS[username]["permissions"]
                st.success(f"Welcome, {st.session_state.name}!")
                st.rerun()
            else:
                st.error("Invalid username or password.")

# --------------
# MAIN APP AFTER LOGIN
# --------------
# Placeholder for session state
if "tool_file" not in st.session_state:
    st.session_state.tool_file = None
if "data_file" not in st.session_state:
    st.session_state.data_file = None
if "label_colname" not in st.session_state:
    st.session_state.label_colname = None

if "survey_df" not in st.session_state:
    st.session_state.survey_df = None
if "choices_df" not in st.session_state:
    st.session_state.choices_df = None
if "table_preview" not in st.session_state:
    st.session_state.table_preview = None

if st.session_state.logged_in:
    st.sidebar.success(f"Welcome, {st.session_state.name}")
    if st.sidebar.button("Logout"):
        logout()
        st.rerun()

    # Navigation Tabs
    tab = st.sidebar.radio("Navigation", [
        "Read Me", "Input Data", 
        "Singular Table Output / Variance", 
        "Singular Graph Output"
    ])

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
            Variance tool by Nestor Cheryba – nestor.cheryba@reach-initiative.org  
            """)

    elif tab == "Input Data":
        st.title("📁 Input Data")
        col1, col2 = st.columns(2)
        with col1:
            data_file = st.file_uploader("Upload Cleaned Data (Excel)", type="xlsx")
            st.session_state.data_file = data_file
        with col2:
            tool_file = st.file_uploader("Upload Kobo Tool (Excel)", type="xlsx")
            st.session_state.tool_file = tool_file

        if tool_file:
            try:
                # Read the "survey" sheet as text
                df_survey = pd.read_excel(tool_file, sheet_name="survey", dtype=str)

                # Extract columns that start with "label::"
                label_cols = [col for col in df_survey.columns if col.startswith("label::")]

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

        if st.session_state.tool_file or st.session_state.data_file:
            df_survey = pd.read_excel(st.session_state.tool_file, sheet_name="survey", dtype=str)
            df_choices = pd.read_excel(st.session_state.tool_file, sheet_name="choices", dtype=str)
            st.session_state.survey_df = df_survey
            st.session_state.choices_df = df_choices
            col1, col2, col3 = st.columns(3)

            with col1:
                if st.session_state.tool_file:
                    if st.session_state.data_file:
                        try:
                            sheet_names = pd.ExcelFile(st.session_state.data_file).sheet_names
                            selected_sheet = st.selectbox("Select Sheet", sheet_names)
                        except Exception as e:
                            st.error(f"Error reading data file: {e}")

            with col2:
                if st.session_state.data_file and 'selected_sheet' in locals():
                    try:
                        df_data = pd.read_excel(st.session_state.data_file, sheet_name=selected_sheet, dtype=str)
                        metadata_cols = ["start", "end", "today", "deviceid", "audit", "audit_URL", "auditURL"]
                        df_data = df_data[[col for col in df_data.columns if col.lower() not in metadata_cols]]

                        non_empty_cols = [col for col in df_data.columns if not df_data[col].isna().all()]
                        variable = st.selectbox("Select Variable", non_empty_cols)
                    except Exception as e:
                        st.error(f"Error loading sheet data: {e}")
                
            with col3:
                if st.session_state.data_file and 'selected_sheet' in locals():
                    try:
                        df_data = pd.read_excel(st.session_state.data_file, sheet_name=selected_sheet, dtype=str)
                        metadata_cols = ["start", "end", "today", "deviceid", "audit", "audit_URL"]
                        df_data = df_data[[col for col in df_data.columns if col.lower() not in metadata_cols]]

                        non_empty_cols = [col for col in df_data.columns if not df_data[col].isna().all()]

                        disagg_options = ["No disaggregation"] + [col for col in non_empty_cols if col != variable]
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
            
            if st.session_state.data_file and st.session_state.tool_file and 'variable' in locals():
                try:
                    df_data = pd.read_excel(st.session_state.data_file, sheet_name=selected_sheet, dtype=str)
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
                    if var_type == "select_one":
                        ctab = pd.crosstab(df_data[disaggregation], df_data[variable], margins=False, dropna=False)
                        ctab.index.name = disaggregation
                        ctab.reset_index(inplace=True)
                        ctab["num_samples"] = ctab.drop(columns=[disaggregation]).sum(axis=1)
                        for col in ctab.columns[1:-1]:
                            ctab[col] = (ctab[col] / ctab["num_samples"] * 100).round(2).astype(str) + "%"
                        result = ctab

                    elif var_type == "select_multiple":
                        sm_cols = [col for col in df_data.columns if col.startswith(variable + "/")]
                        df_sm = df_data[[disaggregation] + sm_cols].copy()
                        df_sm = df_sm.rename(columns=lambda c: c.replace("/", "___"))
                        sm_cols = [c.replace("/", "___") for c in sm_cols]
                        df_sm[sm_cols] = df_sm[sm_cols].apply(pd.to_numeric, errors='coerce')
                        grouped = df_sm.groupby(disaggregation)[sm_cols].agg(['mean', 'count'])
                        grouped.columns = [f"{col}_pct" if stat == 'mean' else "num_samples" for col, stat in grouped.columns]
                        grouped = grouped.reset_index()
                        for col in grouped.columns:
                            if col.endswith("_pct"):
                                grouped[col] = (grouped[col] * 100).round(2).astype(str) + "%"
                        result = grouped

                    elif var_type == "numeric":
                        df_data[variable] = pd.to_numeric(df_data[variable], errors='coerce')
                        result = df_data.groupby(disaggregation)[variable].agg(['count', 'mean', 'median', 'min', 'max']).reset_index()
                        result = result.rename(columns={
                            'count': 'num_samples',
                            'mean': 'mean',
                            'median': 'median',
                            'min': 'min',
                            'max': 'max'
                        })
                        result[['mean', 'median', 'min', 'max']] = result[['mean', 'median', 'min', 'max']].round(2)

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
