"""
maps_germany.py
===============
Generates choropleth maps of Ukrainian refugee inflows and
vote-share changes 2021->2025 at German Kreis (district) level.

Outputs
-------
maps/01_inflow_per1000.png   -- Ukrainian inflow per 1,000 pop
maps/02_inflow_count.png     -- Absolute inflow count
maps/03_inflow_share_pct.png -- Inflow as % of population
maps/04_d_farright.png       -- Delta AfD
maps/05_d_right.png          -- Delta CDU/CSU
maps/06_d_centre.png         -- Delta centre parties (SPD+Greens+FDP)
maps/07_d_left.png           -- Delta Linke
maps/08_d_other.png          -- Delta other (BSW + minor)
maps/09_d_turnout.png        -- Delta turnout
grid_vote_2x2.png            -- 2x2 grid: centre / right / left / far-right
grid_immigration_1x2.png     -- 1x2 grid: absolute count / population share

Requirements
------------
pip install geopandas matplotlib mapclassify

Files needed in the same folder as this script
-----------------------------------------------
kreis_full_dataset.csv   -- your main dataset
VG250_KRS.shp            -- BKG Kreis shapefile (+ .dbf, .prj, .shx)
"""

import os
import geopandas as gpd
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
import matplotlib.colors as mcolors
import matplotlib.ticker as ticker
from matplotlib.colors import TwoSlopeNorm
from matplotlib.cm import ScalarMappable
import warnings
warnings.filterwarnings("ignore")

# ── Paths ─────────────────────────────────────────────────────────────────────
SHP_PATH  = "VG250_KRS.shp"
DATA_PATH = "kreis_full_dataset.csv"
MAPS_DIR  = "maps"
os.makedirs(MAPS_DIR, exist_ok=True)

# ── Load and merge ─────────────────────────────────────────────────────────────
print("Loading data...")
gdf = gpd.read_file(SHP_PATH)
gdf["AGS"] = gdf["AGS"].astype(str).str.zfill(5)

df = pd.read_csv(DATA_PATH, dtype={"key": str})
df["key"] = df["key"].str.zfill(5)
df["ukraine_share_pct"] = (df["ukraine"] / df["population"]) * 100

merged = gdf.merge(df, left_on="AGS", right_on="key", how="left")
merged = merged.to_crs(epsg=4326)
print(f"  {len(merged)} districts, {merged['d_farright'].notna().sum()} matched")

# ── Styling ────────────────────────────────────────────────────────────────────
EDGE_COLOR = "white"
EDGE_WIDTH = 0.15
MISS_COLOR = "#cccccc"
FIG_W, FIG_H = 5.5, 7.5
DPI = 180

def add_colorbar(fig, ax, sm, label, fmt="{x:.1f}"):
    cbar = fig.colorbar(sm, ax=ax, fraction=0.032, pad=0.02,
                        shrink=0.72, aspect=22)
    cbar.set_label(label, fontsize=8, labelpad=6)
    cbar.ax.tick_params(labelsize=7)
    cbar.ax.yaxis.set_major_formatter(ticker.StrMethodFormatter(fmt))

def save_map(fig, name):
    path = os.path.join(MAPS_DIR, name)
    fig.savefig(path, dpi=DPI, bbox_inches="tight",
                facecolor="white", edgecolor="none")
    plt.close(fig)
    print(f"  Saved: {path}")

# ── Sequential choropleth ──────────────────────────────────────────────────────
def seq_map(col, title, label, fname, cmap="Blues", fmt="{x:.1f}"):
    fig, ax = plt.subplots(figsize=(FIG_W, FIG_H))
    ax.set_facecolor("#f5f5f5")
    fig.patch.set_facecolor("white")
    vals = merged[col].dropna()
    vmin, vmax = vals.quantile(0.01), vals.quantile(0.99)
    norm = mcolors.Normalize(vmin=vmin, vmax=vmax)
    merged[merged[col].isna()].plot(
        ax=ax, color=MISS_COLOR, linewidth=EDGE_WIDTH, edgecolor=EDGE_COLOR)
    merged[merged[col].notna()].plot(
        ax=ax, column=col, cmap=cmap, norm=norm,
        linewidth=EDGE_WIDTH, edgecolor=EDGE_COLOR)
    add_colorbar(fig, ax, ScalarMappable(norm=norm, cmap=plt.get_cmap(cmap)),
                 label, fmt=fmt)
    ax.set_title(title, fontsize=9.5, fontweight="bold", pad=8)
    ax.axis("off")
    ax.margins(0)
    fig.tight_layout(pad=0.5)
    save_map(fig, fname)

# ── Diverging choropleth ───────────────────────────────────────────────────────
def div_map(col, title, label, fname, cmap="RdBu_r", fmt="{x:+.1f}"):
    fig, ax = plt.subplots(figsize=(FIG_W, FIG_H))
    ax.set_facecolor("#f5f5f5")
    fig.patch.set_facecolor("white")
    vals = merged[col].dropna()
    abs_max = max(abs(vals.quantile(0.01)), abs(vals.quantile(0.99)))
    norm = TwoSlopeNorm(vmin=-abs_max, vcenter=0, vmax=abs_max)
    merged[merged[col].isna()].plot(
        ax=ax, color=MISS_COLOR, linewidth=EDGE_WIDTH, edgecolor=EDGE_COLOR)
    merged[merged[col].notna()].plot(
        ax=ax, column=col, cmap=cmap, norm=norm,
        linewidth=EDGE_WIDTH, edgecolor=EDGE_COLOR)
    add_colorbar(fig, ax, ScalarMappable(norm=norm, cmap=plt.get_cmap(cmap)),
                 label, fmt=fmt)
    ax.set_title(title, fontsize=9.5, fontweight="bold", pad=8)
    ax.axis("off")
    ax.margins(0)
    fig.tight_layout(pad=0.5)
    save_map(fig, fname)

# ── Individual maps ────────────────────────────────────────────────────────────
print("\nGenerating individual maps...")

seq_map("inflow_1000",
        "Ukrainian protection seekers per 1,000 pop (2022)",
        "Per 1,000 population",
        "01_inflow_per1000.png", cmap="Blues")

seq_map("ukraine",
        "Ukrainian protection seekers, absolute count (2022)",
        "Number of persons",
        "02_inflow_count.png", cmap="Blues", fmt="{x:,.0f}")

seq_map("ukraine_share_pct",
        "Ukrainians as share of district population (%, 2022)",
        "Share (%)",
        "03_inflow_share_pct.png", cmap="Blues")

div_map("d_farright",
        "Δ AfD vote share, 2021→2025 (pp)",
        "Percentage points",
        "04_d_farright.png", cmap="Reds")

div_map("d_right",
        "Δ CDU/CSU vote share, 2021→2025 (pp)",
        "Percentage points",
        "05_d_right.png", cmap="RdBu_r")

div_map("d_centre",
        "Δ Centre parties vote share, 2021→2025 (pp)\n(SPD + Greens + FDP)",
        "Percentage points",
        "06_d_centre.png", cmap="RdBu_r")

div_map("d_left",
        "Δ Linke vote share, 2021→2025 (pp)",
        "Percentage points",
        "07_d_left.png", cmap="RdBu_r")

div_map("d_other",
        "Δ Other parties vote share, 2021→2025 (pp)\n(BSW + minor parties)",
        "Percentage points",
        "08_d_other.png", cmap="RdBu_r")

div_map("d_turnout",
        "Δ Voter turnout, 2021→2025 (pp)",
        "Percentage points",
        "09_d_turnout.png", cmap="RdYlGn")

# ── Grid 1: 2x2 vote share changes ────────────────────────────────────────────
print("\nAssembling grids...")

vote_files = [
    "06_d_centre.png",    # top-left:     centre parties
    "05_d_right.png",     # top-right:    CDU/CSU (right)
    "07_d_left.png",      # bottom-left:  Linke (far left)
    "04_d_farright.png",  # bottom-right: AfD (far right)
]

fig, axes = plt.subplots(2, 2, figsize=(11, 14))
fig.patch.set_facecolor("white")
for ax, fname in zip(axes.flat, vote_files):
    ax.imshow(mpimg.imread(os.path.join(MAPS_DIR, fname)))
    ax.axis("off")
plt.subplots_adjust(wspace=0.01, hspace=0.01)
fig.savefig("grid_vote_2x2.png", dpi=180,
            bbox_inches="tight", facecolor="white")
plt.close()
print("  Saved: grid_vote_2x2.png")

# ── Grid 2: 1x2 immigration ───────────────────────────────────────────────────
imm_files = [
    "02_inflow_count.png",      # left:  absolute count
    "03_inflow_share_pct.png",  # right: population share
]

fig, axes = plt.subplots(1, 2, figsize=(11, 8))
fig.patch.set_facecolor("white")
for ax, fname in zip(axes.flat, imm_files):
    ax.imshow(mpimg.imread(os.path.join(MAPS_DIR, fname)))
    ax.axis("off")
plt.subplots_adjust(wspace=0.01)
fig.savefig("grid_immigration_1x2.png", dpi=180,
            bbox_inches="tight", facecolor="white")
plt.close()
print("  Saved: grid_immigration_1x2.png")

print("\nDone.")
