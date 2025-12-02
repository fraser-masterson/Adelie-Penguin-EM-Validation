# Post-Hoc Validation Pipeline for Latent Behavioural Processes

<br />

This code aims to provide a framework for validating and making predictive models using unsupervised machine learning (ML). By using unsupervised ML as the pipeline's foundation, we can capture inter- and intra-individual variability with relatively low resources. We then incorporate post-hoc approaches to develop on the outputs of the unsupervised ML, allowing us to better understand the model, derive performance metrics, and create predictive models for future use.

<br />

Pipeline process:
1. Clean and classify diving behaviours of Adélie penguins using an Expectation Maximisation model.
2. Validate the classifications of the Expectation Maximisation model following frame-based annotation in VANTAGE.
3. Build generalised linear mixed models (GLMMs) from the validation outputs to build probabilistic prediction frameworks.

<br /> 

<img width="827" height="812" alt="image" src="https://github.com/user-attachments/assets/e5231fd8-21a9-43e7-a0f2-7925fe7d13b9" />
