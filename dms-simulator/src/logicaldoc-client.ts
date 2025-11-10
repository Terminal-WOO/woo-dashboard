import axios from "axios";
import FormData from "form-data";
import { DocumentMetadata } from "./document-generator.js";

export class LogicalDOCClient {
  private baseUrl: string;
  private username: string;
  private password: string;
  private sid: string | null = null;

  constructor(baseUrl: string, username: string, password: string) {
    this.baseUrl = baseUrl;
    this.username = username;
    this.password = password;
  }

  /**
   * Login and get session ID (sid)
   */
  private async login(): Promise<string> {
    if (this.sid) {
      return this.sid;
    }

    try {
      // LogicalDOC REST API uses GET method with query parameters
      const response = await axios.get(
        `${this.baseUrl}/services/rest/auth/login`,
        {
          params: {
            u: this.username,
            pw: this.password,
          },
          headers: {
            Accept: "application/json",
          },
        },
      );

      this.sid = response.data;
      return this.sid;
    } catch (error: any) {
      console.error(
        "LogicalDOC login error:",
        error.response?.data || error.message,
      );
      throw new Error("Failed to login to LogicalDOC");
    }
  }

  /**
   * Upload a document to LogicalDOC
   */
  async uploadDocument(
    pdfBuffer: Buffer,
    metadata: DocumentMetadata,
  ): Promise<{ id: string; success: boolean }> {
    const sid = await this.login();

    const form = new FormData();

    // Create filename
    const filename = `${metadata.date.toISOString().split("T")[0]}_${metadata.type}_${metadata.category.replace(/\s+/g, "_")}.pdf`;

    form.append("document", pdfBuffer, {
      filename,
      contentType: "application/pdf",
    });

    // LogicalDOC parameters
    form.append("sid", sid);
    form.append("folderId", "5"); // Default workspace root folder
    form.append("filename", filename);
    form.append("language", "en");

    // Add tags as a comma-separated string
    if (metadata.tags && metadata.tags.length > 0) {
      form.append("tags", metadata.tags.join(","));
    }

    try {
      const response = await axios.post(
        `${this.baseUrl}/services/rest/document/upload`,
        form,
        {
          headers: {
            ...form.getHeaders(),
            Accept: "application/json",
          },
          maxBodyLength: Infinity,
          maxContentLength: Infinity,
        },
      );

      const documentId = response.data.id || response.data;

      // Update document metadata (title, date, custom attributes)
      await this.updateDocumentMetadata(documentId, metadata);

      return {
        id: String(documentId),
        success: true,
      };
    } catch (error: any) {
      console.error(
        "LogicalDOC upload error:",
        error.response?.data || error.message,
      );
      throw error;
    }
  }

  /**
   * Update document metadata after upload
   */
  private async updateDocumentMetadata(
    documentId: string,
    metadata: DocumentMetadata,
  ): Promise<void> {
    const sid = await this.login();

    try {
      // Get document details first
      const docResponse = await axios.get(
        `${this.baseUrl}/services/rest/document/getDocument`,
        {
          params: { sid, docId: documentId },
        },
      );

      const document = docResponse.data;

      // Update with new metadata
      document.customId = `${metadata.type}-${metadata.category}`;
      document.date = metadata.date.toISOString().split("T")[0];
      document.title = metadata.title;

      // Set custom attributes for type, category, and author
      if (!document.attributes) {
        document.attributes = [];
      }

      document.attributes.push(
        { name: "type", stringValue: metadata.type },
        { name: "category", stringValue: metadata.category },
        { name: "author", stringValue: metadata.author },
      );

      // Update the document
      await axios.put(
        `${this.baseUrl}/services/rest/document/update`,
        document,
        {
          params: { sid },
          headers: {
            "Content-Type": "application/json",
          },
        },
      );
    } catch (error: any) {
      console.warn(
        "Failed to update document metadata:",
        error.response?.data || error.message,
      );
      // Don't throw - document is already uploaded
    }
  }

  /**
   * Test connection to LogicalDOC
   */
  async testConnection(): Promise<boolean> {
    try {
      await this.login();
      return true;
    } catch (error) {
      return false;
    }
  }

  /**
   * Logout and invalidate session
   */
  async logout(): Promise<void> {
    if (!this.sid) {
      return;
    }

    try {
      await axios.post(`${this.baseUrl}/services/rest/auth/logout`, null, {
        params: { sid: this.sid },
      });
      this.sid = null;
    } catch (error) {
      console.warn("Logout error:", error);
      this.sid = null;
    }
  }
}
