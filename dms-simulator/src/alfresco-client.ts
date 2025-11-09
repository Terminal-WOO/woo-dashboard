import axios from 'axios';
import FormData from 'form-data';
import { DocumentMetadata } from './document-generator.js';

export class AlfrescoClient {
  private baseUrl: string;
  private username: string;
  private password: string;
  private ticket?: string;

  constructor(baseUrl: string, username: string, password: string) {
    this.baseUrl = baseUrl;
    this.username = username;
    this.password = password;
  }

  async login(): Promise<void> {
    try {
      const response = await axios.post(
        `${this.baseUrl}/alfresco/api/-default-/public/authentication/versions/1/tickets`,
        {
          userId: this.username,
          password: this.password,
        }
      );
      this.ticket = response.data.entry.id;
    } catch (error: any) {
      console.error('Alfresco login failed:', error.response?.data || error.message);
      throw new Error('Alfresco authentication failed');
    }
  }

  async uploadDocument(
    pdfBuffer: Buffer,
    metadata: DocumentMetadata
  ): Promise<{ id: string; success: boolean }> {
    if (!this.ticket) {
      await this.login();
    }

    const form = new FormData();

    const filename = `${metadata.date.toISOString().split('T')[0]}_${metadata.type}_${metadata.category.replace(/\s+/g, '_')}.pdf`;

    form.append('filedata', pdfBuffer, {
      filename,
      contentType: 'application/pdf',
    });

    // Alfresco metadata
    form.append('name', filename);
    form.append('cm:title', metadata.title);
    form.append('cm:description', metadata.summary);
    form.append('cm:author', metadata.author);

    // Add to a specific folder (create if not exists)
    const folderId = await this.ensureFolder(metadata.category);

    try {
      const response = await axios.post(
        `${this.baseUrl}/alfresco/api/-default-/public/alfresco/versions/1/nodes/${folderId}/children`,
        form,
        {
          headers: {
            ...form.getHeaders(),
            'Authorization': `Basic ${Buffer.from(`${this.username}:${this.password}`).toString('base64')}`,
          },
          params: {
            alf_ticket: this.ticket,
          },
          maxBodyLength: Infinity,
          maxContentLength: Infinity,
        }
      );

      return {
        id: response.data.entry.id,
        success: true,
      };
    } catch (error: any) {
      console.error('Alfresco upload error:', error.response?.data || error.message);

      // Token might be expired, retry once
      if (error.response?.status === 401) {
        await this.login();
        return this.uploadDocument(pdfBuffer, metadata);
      }

      throw error;
    }
  }

  private async ensureFolder(folderName: string): Promise<string> {
    if (!this.ticket) {
      await this.login();
    }

    try {
      // Search for existing folder in My Files
      const searchResponse = await axios.get(
        `${this.baseUrl}/alfresco/api/-default-/public/alfresco/versions/1/nodes/-my-/children`,
        {
          headers: {
            'Authorization': `Basic ${Buffer.from(`${this.username}:${this.password}`).toString('base64')}`,
          },
          params: {
            alf_ticket: this.ticket,
            where: `(name='${folderName}' AND isFolder=true)`,
          },
        }
      );

      if (searchResponse.data.list.entries.length > 0) {
        return searchResponse.data.list.entries[0].entry.id;
      }

      // Create folder
      const createResponse = await axios.post(
        `${this.baseUrl}/alfresco/api/-default-/public/alfresco/versions/1/nodes/-my-/children`,
        {
          name: folderName,
          nodeType: 'cm:folder',
          properties: {
            'cm:title': folderName,
            'cm:description': `Automatisch aangemaakte folder voor ${folderName}`,
          },
        },
        {
          headers: {
            'Authorization': `Basic ${Buffer.from(`${this.username}:${this.password}`).toString('base64')}`,
            'Content-Type': 'application/json',
          },
          params: {
            alf_ticket: this.ticket,
          },
        }
      );

      return createResponse.data.entry.id;
    } catch (error: any) {
      console.error('Alfresco folder error:', error.response?.data || error.message);
      // Return -my- as fallback
      return '-my-';
    }
  }

  async testConnection(): Promise<boolean> {
    try {
      await this.login();
      const response = await axios.get(
        `${this.baseUrl}/alfresco/api/-default-/public/alfresco/versions/1/nodes/-my-`,
        {
          headers: {
            'Authorization': `Basic ${Buffer.from(`${this.username}:${this.password}`).toString('base64')}`,
          },
          params: {
            alf_ticket: this.ticket,
          },
        }
      );
      return response.status === 200;
    } catch (error) {
      return false;
    }
  }
}
