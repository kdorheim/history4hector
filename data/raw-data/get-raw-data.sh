# Remove any existing .csv, .xlsx, and .zip files
rm -f *.csv *.xlsx *.zip README_aggregate_emissions.txt *.DS_Store

# Download files from URLs in raw-data.urls.txt
wget -i raw-data.urls.txt

# Unzip any .zip files and remove them after extraction
for file in *.zip; do
    unzip "$file" && rm "$file"
done

