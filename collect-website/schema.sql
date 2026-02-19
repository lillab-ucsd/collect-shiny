

CREATE TABLE metadata (
    response_id VARCHAR(255) PRIMARY KEY,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    -- Survey Metadata (6 columns)
    progress INTEGER,
    duration_in_seconds INTEGER,
    finished BOOLEAN,
    recorded_date TIMESTAMP,
    distribution_channel VARCHAR(100),
    user_language VARCHAR(10),

    -- Consent (1 column)
    consent_form TEXT
)

CREATE TABLE freelist_collection (
    response_id VARCHAR(255) PRIMARY KEY,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    -- Collection Basics (8 columns)
    collect_yn VARCHAR(10),
    collect_freelist_1 VARCHAR(255),
    collect_freelist_2 VARCHAR(255),
    collect_freelist_3 VARCHAR(255),
    collect_freelist_4 VARCHAR(255),
    collect_freelist_5 VARCHAR(255),
    collect_freelist_7 VARCHAR(255),
    favorite VARCHAR(255),
    
    -- Collection 1 Details (8 columns)
    collection_origin_1 TEXT,
    activities_1 TEXT,
    collect_friends_1 VARCHAR(10),
    interest_questions_1_1 INTEGER,
    interest_questions_1_2 INTEGER,
    interest_questions_1_3 INTEGER,
    interest_questions_1_4 INTEGER,
    collection_reason_1 TEXT,
    ask_child_1 TEXT,
    
    -- Collection 2 Details (8 columns)
    collection_origin_2 TEXT,
    activities_2 TEXT,
    collect_friends_2 VARCHAR(10),
    interest_questions_2_1 INTEGER,
    interest_questions_2_2 INTEGER,
    interest_questions_2_3 INTEGER,
    interest_questions_2_4 INTEGER,
    collection_reason_2 TEXT,
    ask_child_2 TEXT,
    
    -- Collection 3 Details (8 columns)
    collection_origin_3 TEXT,
    activities_3 TEXT,
    collect_friends_3 VARCHAR(10),
    interest_questions_3_1 INTEGER,
    interest_questions_3_2 INTEGER,
    interest_questions_3_3 INTEGER,
    interest_questions_3_4 INTEGER,
    collection_reason_3 TEXT,
    ask_child_3 TEXT,
    
    -- Collection 4 Details (8 columns)
    collection_origin_4 TEXT,
    activities_4 TEXT,
    collect_friends_4 VARCHAR(10),
    interest_questions_4_1 INTEGER,
    interest_questions_4_2 INTEGER,
    interest_questions_4_3 INTEGER,
    interest_questions_4_4 INTEGER,
    collection_reason_4 TEXT,
    ask_child_4 TEXT,
    
    -- Collection 5 Details (8 columns)
    collection_origin_5 TEXT,
    activities_5 TEXT,
    collect_friends_5 VARCHAR(10),
    interest_questions_5_1 INTEGER,
    interest_questions_5_2 INTEGER,
    interest_questions_5_3 INTEGER,
    interest_questions_5_4 INTEGER,
    collection_reason_5 TEXT,
    ask_child_5 TEXT,
    
    -- Collection 6 Details (8 columns)
    collection_origin_6 TEXT,
    activities_6 TEXT,
    collect_friends_6 VARCHAR(10),
    interest_questions_6_1 INTEGER,
    interest_questions_6_2 INTEGER,
    interest_questions_6_3 INTEGER,
    interest_questions_6_4 INTEGER,
    collection_reason_6 TEXT,
    ask_child_6 TEXT,
)

CREATE TABLE checklist_collection (
    response_id VARCHAR(255) PRIMARY KEY,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    -- Collection Checklist (40 columns)
    collect_checklist_1 VARCHAR(255),
    collect_checklist_2 VARCHAR(255),
    collect_checklist_3 VARCHAR(255),
    collect_checklist_4 VARCHAR(255),
    collect_checklist_5 VARCHAR(255),
    collect_checklist_6 VARCHAR(255),
    collect_checklist_7 VARCHAR(255),
    collect_checklist_8 VARCHAR(255),
    collect_checklist_9 VARCHAR(255),
    collect_checklist_10 VARCHAR(255),
    collect_checklist_11 VARCHAR(255),
    collect_checklist_12 VARCHAR(255),
    collect_checklist_13 VARCHAR(255),
    collect_checklist_14 VARCHAR(255),
    collect_checklist_15 VARCHAR(255),
    collect_checklist_16 VARCHAR(255),
    collect_checklist_17 VARCHAR(255),
    collect_checklist_18 VARCHAR(255),
    collect_checklist_19 VARCHAR(255),
    collect_checklist_20 VARCHAR(255),
    collect_checklist_21 VARCHAR(255),
    collect_checklist_22 VARCHAR(255),
    collect_checklist_23 VARCHAR(255),
    collect_checklist_24 VARCHAR(255),
    collect_checklist_25 VARCHAR(255),
    collect_checklist_26 VARCHAR(255),
    collect_checklist_27 VARCHAR(255),
    collect_checklist_28 VARCHAR(255),
    collect_checklist_29 VARCHAR(255),
    collect_checklist_30 VARCHAR(255),
    collect_checklist_31 VARCHAR(255),
    collect_checklist_32 VARCHAR(255),
    collect_checklist_33 VARCHAR(255),
    collect_checklist_34 VARCHAR(255),
    collect_checklist_35 VARCHAR(255),
    collect_checklist_36 VARCHAR(255),
    collect_checklist_37 VARCHAR(255),
    collect_checklist_38 VARCHAR(255),
    collect_checklist_39 VARCHAR(255),
    collect_checklist_40 VARCHAR(255),
    
    -- Checklist Text Fields (15 columns)
    collect_checklist_1_text TEXT,
    collect_checklist_6_text TEXT,
    collect_checklist_7_text TEXT,
    collect_checklist_8_text TEXT,
    collect_checklist_9_text TEXT,
    collect_checklist_19_text TEXT,
    collect_checklist_20_text TEXT,
    collect_checklist_26_text TEXT,
    collect_checklist_27_text TEXT,
    collect_checklist_28_text TEXT,
    collect_checklist_30_text TEXT,
    collect_checklist_36_text TEXT,
    collect_checklist_37_text TEXT,
    collect_checklist_38_text TEXT,
    collect_checklist_40_text TEXT,

    -- Open Question (1 column)
    open_question TEXT,
)

CREATE TABLE demographics (
    response_id VARCHAR(255) PRIMARY KEY,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    -- Demographics - Child (5 columns)
    age INTEGER,
    gender VARCHAR(50),
    l1 VARCHAR(100),
    l1percent VARCHAR(20),
    other_languages VARCHAR(255),
    
    -- Demographics - Race (8 columns)
    race_1 VARCHAR(100),
    race_2 VARCHAR(100),
    race_3 VARCHAR(100),
    race_4 VARCHAR(100),
    race_5 VARCHAR(100),
    race_6 VARCHAR(100),
    race_7 VARCHAR(100),
    race_8 VARCHAR(100),
    race_7_text TEXT,
    
    -- Development Concerns (2 columns)
    dev_concern VARCHAR(10),
    hv_explain TEXT,
    
    -- Parent Education (3 columns)
    parent_education VARCHAR(255),
    parent_education_8_text TEXT,
    parent_education_10_text TEXT,
    
    -- Location & Income (2 columns)
    country VARCHAR(100),
    household_income VARCHAR(100),
    
    -- Identifiers & Computed Fields (5 columns)
    multi_rid VARCHAR(255),
    participant_id VARCHAR(255),
    age_num INTEGER,
    gender_clean VARCHAR(50),
    age_bin VARCHAR(50)
)