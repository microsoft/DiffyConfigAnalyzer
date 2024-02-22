[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

# Diffy: data-driven bug finding for configurations

## Introduction
Diffy is a push-button configuration analyzer that detects likely bugs in arbitrary JSON configurations. It learns a common template from a set of similar configurations and employs unsupervised learning to identify anomalous template parameters as likely bugs.

For more details, please refer to our PLDI '24 paper.

## Build

Install [dotnet](https://dotnet.microsoft.com/en-us/) with support for [F#](https://fsharp.org/). From the main repository folder, execute the `make.ps1` script on Windows or `make.sh` on Mac/Linux to build. A binary will be created in a `build` directory. As an example, this README focuses on Windows.

## Getting Started

To get started, let's run Diffy on some example configurations -- 5G radio unit (RU) configurations in the `examples\5GvRAN` directory. Run the following command:

```
.\build\diffy.exe --dir .\examples\5GvRAN\ --confidence 0.8 --tokens "num:[0-9]+" --token-costs "0.2" --outliers-path outliers
```

Let's break down the command:
* `--confidence 0.8` sets the confidence threshold at 0.8 out of 1. A higher confidence level (closer to 1) necessitates more substantial evidence for deviations from normal configurations.
* `--tokens "num:[0-9]+" --token-costs "0.2"` define a token and its associated cost. In this case, we define a token `num` to match any number, assigned with a cost of 0.2.
* `--outliers-path outliers` instructs Diffy to save the detected outliers in `outliers.csv`.

Opening the `outliers.csv` file, you can see lines such as:

```
Name,Expression,Score
retro_new_firmware.json,present(/RRH_PTPV2_VLAN_ID) = 'True',0.9054418774835514
retro_rf_general_ctrl.json,value(/RRH_RF_GENERAL_CTRL/elt[0]/param[0]:0x[num]) = '0',0.9054418774835514
retro_ptp_ip.json,value(/RRH_PTPV2_GRAND_MASTER_IP/param[0]:192.16[num].[num].1[num]) = '7',0.9054418774835514   retro_ptp_ip.json,pow2(/RRH_PTPV2_GRAND_MASTER_IP/param[0]:192.16[num].[num].1[num]) = 'False',0.9054418774835514
retro_new_firmware.json,present(/RRH_PTPV2_OBSERVATION_TIME) = 'False',0.9054418774835514                        retro_new_firmware.json,value(/RRH_PTPV2_GRAND_MASTER_MODE/param[0]:[num]) = '2',0.9054418774835514
retro_new_firmware.json,pow2(/RRH_PTPV2_GRAND_MASTER_MODE/param[0]:[num]) = 'True',0.9054418774835514
retro_new_firmware.json,present(/RRH_PTPV2_JITTER_LEVEL) = 'True',0.9054418774835514
retro_ptp_domain.json,value(/RRH_PTPV2_SUB_DOMAIN_NUM/param[0]:[num]) = '24',0.9054418774835514
retro_ptp_ip.json,value(/RRH_PTPV2_GRAND_MASTER_IP/param[2]:192.16[num].[num].1[num]) = '50',0.9045429287657311
retro_frequency.json,value(/RRH_LO_FREQUENCY_KHZ/param[0]:3[num]) = '929700',0.8929374914650378
```

Each line lists the file name, a flagged anomaly, and its anomaly score. For example, the following line 
```
retro_frequency.json,value(/RRH_LO_FREQUENCY_KHZ/param[0]:3[num]) = '929700',0.8929374914650378
```
indicates that in a particular file, the RU frequency was set to `3929700` (kHz). This value is flagged as unusual with a high anomaly score of 0.89, suggesting a significant deviation from typical configurations.

To view more details about the anomalies and to generate a visual summary, run the same command as before except with an additional `--outliers-html` flag:

```
.\build\diffy.exe --dir .\examples\5GvRAN\ --confidence 0.8 --tokens "num:[0-9]+" --token-costs "0.2" --outliers-path outliers --outliers-html 
```
This generates a file `outliers.html`. When opened in a browser, it will show a visual summary of each configuration outlier. For the discussed anomaly, we see the following summary:

```
{
  "Pattern": "3[num]",
  "Parameters": [
    {
      "352260": 12,
      "460260": 37,
      "929700": 1
    }
  ]
}
```
Among a total of 50 configurations, 12 files set the frequency to `3352260`, 37 files set the frequency to `3460260`, and only a single file had the frequency set to `3929700`. This is a clear outlier and thus marked by Diffy as a likely misconfiguration.

## Command Line Arguments

You can view all of the Diffy command line arguments:

```
PS > .\build\diffy.exe --help
Diffy.Cli 1.0.0
Copyright (C) 2024 Diffy.Cli

  --dir                          Required. The directory containing the configurations.

  --format                       The configuration file format (default json).

  --print                        Print progress of the tool.

  --exclude                      Regex patterns for tree paths to exclude (example: /some/.*/path

  --include                      Regex patterns for tree paths to include (Default: .*) (example: /some/.*/path

  --group-threshold              A group thresholding value between 0.0 and 1.0.

  --list-threshold               A list thresholding value that determines which list to prefer.

  --tokens                       A set of tokens given as regular expressions.

  --token-costs                  The costs for each of the tokens between 0.0 and 1.0.

  --stats                        Show stats only and no output.

  --max-list-size                The maximum number of elements in a list to template.

  --count-only                   Includes only the group count and not members in the results.

  --loose-key-match              Allows loose key matching in records.

  --disable-repeat               Disables the use of repeat templates.

  --parameters                   Dump parameter values in the output.

  --support                      The minimum support needed to flag an outlier (default 0.2).

  --confidence                   The minimum confidence needed to flag an outlier (default 0.7).

  --outliers-path                The outliers output file path (without extension).

  --outliers-include-template    Whether to include the template text in the CSV file.

  --outliers-html                Whether to output outliers in HTML format for visual summary instead of CSV.

  --template-path                The output file path (without extension).

  --filter-outliers              A set of filters to apply to outlier descriptions.

  --help                         Display this help screen.

  --version                      Display version information.
```

## Contributing

This project welcomes contributions and suggestions.  Most contributions require you to agree to a
Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us
the rights to use your contribution. For details, visit https://cla.opensource.microsoft.com.

When you submit a pull request, a CLA bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately (e.g., status check, comment). Simply follow the instructions
provided by the bot. You will only need to do this once across all repos using our CLA.

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.

## Trademarks

This project may contain trademarks or logos for projects, products, or services. Authorized use of Microsoft 
trademarks or logos is subject to and must follow 
[Microsoft's Trademark & Brand Guidelines](https://www.microsoft.com/en-us/legal/intellectualproperty/trademarks/usage/general).
Use of Microsoft trademarks or logos in modified versions of this project must not cause confusion or imply Microsoft sponsorship.
Any use of third-party trademarks or logos are subject to those third-party's policies.
